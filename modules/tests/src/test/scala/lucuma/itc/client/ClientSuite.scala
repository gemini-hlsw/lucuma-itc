// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client

import cats.effect._
import cats.implicits._
import com.comcast.ip4s._
import lucuma.itc.tests.FixedItc
import lucuma.itc.client.ItcClient
import munit.CatsEffectSuite
import natchez.Trace.Implicits.noop
import org.http4s._
import org.http4s.blaze.server.BlazeServerBuilder
//import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.Server
import org.http4s.server.websocket.WebSocketBuilder2
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import scala.concurrent.duration._


trait ClientSuite extends CatsEffectSuite {

  private implicit val log: Logger[IO] =
    Slf4jLogger.getLoggerFromClass(getClass)

  private val httpApp: Resource[IO, WebSocketBuilder2[IO] => HttpApp[IO]] =
    Resource.eval(lucuma.itc.tests.app(FixedItc))

  private val server: Resource[IO, Server] =
//    Resource.make(IO.println("  • Server starting..."))(_ => IO.println("  • Server stopped.")) *>
    httpApp.flatMap { app =>
      BlazeServerBuilder[IO]
        .withHttpWebSocketApp(app)
        .bindAny()
        .resource

//        .flatTap(_ => Resource.eval(IO.println("  • Server started.")))

//      EmberServerBuilder
//        .default[IO]
//        .withHost(ipv4"0.0.0.0")
//        .withPort(Port.fromInt(cfg.port).get)
//        .withHttpWebSocketApp(app)
//        .build
    }

  private val serverFixture: Fixture[Server] =
    ResourceSuiteLocalFixture("server", server)

  override def munitFixtures = List(serverFixture)

  def clientTest(
    name:       String
  )(
    clientTest: ItcClient[IO] => IO[Unit]
  ): Unit =
    test(name) {
      Resource
        .eval {
          for {
            srv <- IO(serverFixture())
            uri  = srv.baseUri  /  "graphql" // "itc"
            cli <- ItcClient.create[IO](uri)
          } yield {
            println(uri)
            cli
          }
        }
        .use { client =>
          clientTest(client)
        }
    }

}
