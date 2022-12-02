// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import buildinfo.BuildInfo
import cats.Applicative
import cats.Functor
import cats.Parallel
import cats.data.Kleisli
import cats.effect._
import cats.syntax.all._
import com.comcast.ip4s._
import dev.profunktor.redis4cats.Redis
import dev.profunktor.redis4cats.data.RedisCodec
import dev.profunktor.redis4cats.log4cats.*
import lucuma.graphql.routes.GrackleGraphQLService
import lucuma.graphql.routes.Routes
import lucuma.itc.ItcImpl
import lucuma.itc.legacy.LocalItc
import lucuma.itc.service.config.ExecutionEnvironment._
import lucuma.itc.service.config._
import natchez.EntryPoint
import natchez.Trace
import natchez.honeycomb.Honeycomb
import natchez.http4s.NatchezMiddleware
import natchez.http4s.implicits._
import natchez.log.Log
import org.http4s.HttpApp
import org.http4s._
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.headers.`Cache-Control`
import org.http4s.implicits._
import org.http4s.server.Server
import org.http4s.server.middleware.CORS
import org.http4s.server.middleware.CORSPolicy
import org.http4s.server.middleware.GZip
import org.http4s.server.middleware.{Logger => Http4sLogger}
import org.http4s.server.staticcontent._
import org.http4s.server.websocket.WebSocketBuilder2
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import java.io.File
import java.io.FileFilter
import java.net.URL
import java.net.URLClassLoader
import scala.concurrent.duration._
import scala.util.Try

// #server
object Main extends IOApp with ItcCacheOrRemote {
  val ServiceName     = "lucuma-itc"
  val DefaultCacheTTL = 6.hours

  /** A startup action that prints a banner. */
  def banner[F[_]: Applicative: Logger](cfg: Config): F[Unit] =
    val banner =
      s"""|
            |   / /_  _________  ______ ___  ____ _      (_) /______
            |  / / / / / ___/ / / / __ `__ \\/ __ `/_____/ / __/ ___/
            | / / /_/ / /__/ /_/ / / / / / / /_/ /_____/ / /_/ /__
            |/_/\\__,_/\\___/\\__,_/_/ /_/ /_/\\__,_/     /_/\\__/\\___/
            |
            | redis-url ${cfg.redisUrl}
            | port ${cfg.port}
            | data checksum ${BuildInfo.ocslibHash}
            |
            |""".stripMargin
    banner.linesIterator.toList.traverse_(Logger[F].info(_))

  /** A middleware that adds CORS headers. In production the origin must match the cookie domain. */
  def cors(env: ExecutionEnvironment, domain: Option[String]): CORSPolicy =
    env match
      case Local | Review | Staging =>
        CORS.policy
      case Production               =>
        CORS.policy
          .withAllowOriginHostCi(domain.contains)

  /**
   * A resource that yields a Natchez tracing entry point, either a Honeycomb endpoint if `config`
   * is defined, otherwise a log endpoint.
   */
  def entryPointResource[F[_]: Sync: Logger](
    config: Option[HoneycombConfig]
  ): Resource[F, EntryPoint[F]] =
    config.fold(Log.entryPoint(ServiceName).pure[Resource[F, *]]) { cfg =>
      Honeycomb.entryPoint(ServiceName) { cb =>
        Sync[F].delay {
          cb.setWriteKey(cfg.writeKey)
          cb.setDataset(cfg.dataset)
          cb.build()
        }
      }
    }

  def cacheMiddleware[F[_]: Functor](service: HttpRoutes[F]): HttpRoutes[F] = Kleisli {
    (req: Request[F]) =>
      service(req).map {
        case Status.Successful(resp) =>
          resp.putHeaders(
            `Cache-Control`(CacheDirective.public, CacheDirective.`max-age`(DefaultCacheTTL))
          )
        case resp                    =>
          resp
      }
  }

  def serverResource[F[_]: Async](
    app: WebSocketBuilder2[F] => HttpApp[F],
    cfg: Config
  ): Resource[F, Server] =
    // Spin up the server ...
    EmberServerBuilder
      .default[F]
      .withHost(ipv4"0.0.0.0")
      .withPort(Port.fromInt(cfg.port).get)
      .withHttpWebSocketApp(app)
      .build

  def routes[F[_]: Async: Concurrent: Logger: Parallel: Trace](
    cfg: Config,
    itc: LocalItc
  ): Resource[F, WebSocketBuilder2[F] => HttpRoutes[F]] =
    for
      itc     <- Resource.eval(ItcImpl.build(itc).pure[F])
      redis   <- Redis[F].simple(cfg.redisUrl.toString, RedisCodec.gzip(RedisCodec.Bytes))
      _       <- Resource.eval(checkVersionToPurge[F](redis, itc))
      _       <- Resource.eval(Logger[F].info("START"))
      mapping <- Resource.eval(ItcMapping(cfg.environment, redis, itc))
    yield wsb =>
      // Routes for the ITC GraphQL service
      NatchezMiddleware.server(
        GZip(
          cors(cfg.environment, none)(
            cacheMiddleware(
              Routes.forService(_ => GrackleGraphQLService[F](mapping).some.pure[F], wsb, "itc")
            )
          )
        )
      )

  // Custom class loader to give prioritiy to the jars in the urls over the parent classloader
  class ReverseClassLoader(urls: Array[URL], parent: ClassLoader)
      extends URLClassLoader(urls, parent) {
    override def loadClass(name: String): Class[?] = {
      println(s"Requested $name")
      // First check whether it's already been loaded, if so use it
      if (name.startsWith("java.lang")) super.loadClass(name)
      else {
        Option(findLoadedClass(name)).getOrElse {

          // Not loaded, try to load it
          Try(findClass(name)).getOrElse {
            // If not found locally, use normal parent delegation in URLClassloader
            super.loadClass(name);
          }
        }
      }
    }
  }

  // Build a custom class loader to read and call the legacy ocs2 libs
  // without affecting the current classes. This is mostly because ocs2 uses scala 2.11
  // and it will conflict with the current scala 3 classes
  def legacyItcLoader[F[_]: Sync: Logger](config: Config): F[LocalItc] =
    Sync[F]
      .delay {
        val dir      =
          if (config.dyno.isDefined) "modules/service/target/universal/stage/ocslib" else "ocslib"
        // println(new File(dir).getAbsolutePath())
        val jarFiles =
          new File(dir).listFiles(new FileFilter() {
            override def accept(file: File): Boolean = {
              println(file.getName())
              println(file.exists())
              println(file.getTotalSpace())
              file.getName().endsWith(".jar");
            }
          })
        jarFiles.foreach(println)
        LocalItc(
          new ReverseClassLoader(jarFiles.map(_.toURI.toURL), ClassLoader.getSystemClassLoader())
        )
      }

  /**
   * Our main server, as a resource that starts up our server on acquire and shuts it all down in
   * cleanup, yielding an `ExitCode`. Users will `use` this resource and hold it forever.
   */
  def server[F[_]: Async: Parallel: Logger](cfg: Config): Resource[F, ExitCode] =
    for
      cl <- Resource.eval(legacyItcLoader[F](cfg))
      _  <- Resource.eval(banner(cfg))
      ep <- entryPointResource(cfg.honeycomb)
      ap <- ep.wsLiftR(routes(cfg, cl)).map(_.map(_.orNotFound))
      _  <- serverResource(ap, cfg)
    yield ExitCode.Success

  def run(args: List[String]): IO[ExitCode] =
    for
      cfg              <- Config.config.load[IO]
      given Logger[IO] <- Slf4jLogger.create[IO]
      _                <- server[IO](cfg).use(_ => IO.never[ExitCode])
    yield ExitCode.Success
}
