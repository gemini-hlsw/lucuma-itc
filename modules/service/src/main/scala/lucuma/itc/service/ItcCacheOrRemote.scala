// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import boopickle.DefaultBasic.*
import cats.*
import cats.syntax.all.*
import dev.profunktor.redis4cats.algebra.Flush
import dev.profunktor.redis4cats.algebra.StringCommands
import lucuma.itc.Itc
import lucuma.itc.search.ItcVersions
import lucuma.itc.service.config.ExecutionEnvironment
import lucuma.itc.service.redis.given
import org.typelevel.log4cats.Logger

import java.nio.ByteBuffer
import java.nio.charset.Charset
import scala.concurrent.duration.*

/**
 * Methods to check if a values is on the cache and if not retrieve them from old itc and store them
 * in the cache
 *
 * redis keys are formed with a prefix and a hash of the request redis values are stored in binary
 * via boopickle
 */
trait ItcCacheOrRemote extends Version:
  val KeyCharset = Charset.forName("UTF8")
  // Time to live for entries
  val TTL        = FiniteDuration(10, DAYS)
  val VersionKey = "itc:version".getBytes(KeyCharset)

  /**
   * Generic method to stora a value on redis or request it locally
   */
  private def cacheOrRemote[F[_]: MonadThrow: Logger, A: Hash, B: Pickler](
    a:       A,
    request: A => F[B]
  )(
    prefix:  String,
    redis:   StringCommands[F, Array[Byte], Array[Byte]]
  ): F[B] = {
    val hash     = Hash[A].hash(a)
    val redisKey = s"$prefix:$hash".getBytes(KeyCharset)
    val L        = Logger[F]
    for
      fromRedis <- redis
                     .get(redisKey)
                     .handleErrorWith(e => L.error(e)(s"Error reading $redisKey") *> none.pure[F])
      decoded   <-
        fromRedis
          .flatMap(b => Either.catchNonFatal(Unpickle[B].fromBytes(ByteBuffer.wrap(b))).toOption)
          .pure[F]
      r         <- decoded.map(_.pure[F]).getOrElse(request(a))
      _         <-
        redis
          .setEx(redisKey, Pickle.intoBytes(r).compact().array(), TTL)
          .handleErrorWith(L.error(_)(s"Error writing $redisKey"))
          .whenA(fromRedis.isEmpty || decoded.isEmpty)
    yield r
  }

  private val requestGraph = [F[_]] =>
    (itc: Itc[F]) =>
      (request: GraphRequest) =>
        itc
          .calculateGraph(
            request.targetProfile,
            request.specMode,
            request.constraints,
            request.expTime,
            request.exp
        )

  /**
   * Request a graph
   */
  def graphFromCacheOrRemote[F[_]: MonadThrow: Logger](
    request: GraphRequest
  )(itc:     Itc[F], redis: StringCommands[F, Array[Byte], Array[Byte]]): F[Itc.GraphResult] =
    cacheOrRemote(request, requestGraph(itc))("itc:graph:spec", redis)

  private val requestCalc = [F[_]] =>
    (itc: Itc[F]) =>
      (calcRequest: CalcRequest) =>
        itc
          .calculate(
            calcRequest.targetProfile,
            calcRequest.specMode,
            calcRequest.constraints,
            calcRequest.signalToNoise.value
        )

  /**
   * Request exposure time calculation
   */
  def calcFromCacheOrRemote[F[_]: MonadThrow: Logger](
    calcRequest: CalcRequest
  )(itc:         Itc[F], redis: StringCommands[F, Array[Byte], Array[Byte]]): F[Itc.CalcResultWithVersion] =
    cacheOrRemote(calcRequest, requestCalc(itc))("itc:calc:spec", redis)

  /**
   * Request and store the itc version
   */
  def versionFromCacheOrRemote[F[_]: MonadThrow: Logger](
    environment: ExecutionEnvironment,
    redis:       StringCommands[F, Array[Byte], Array[Byte]],
    itc:         Itc[F]
  ): F[ItcVersions] = {
    val L = Logger[F]
    for
      fromRedis <- redis
                     .get(VersionKey)
                     .handleErrorWith(e => L.error(e)(s"Error reading $VersionKey") *> none.pure[F])
      version   <- fromRedis.fold(
                     itc.itcVersions
                       .map { r =>
                         ItcVersions(version(environment).value, r.some)
                       }
                   )(v => ItcVersions(version(environment).value, String(v, KeyCharset).some).pure[F])
      _         <-
        redis
          .setEx(VersionKey, version.dataVersion.orEmpty.getBytes(KeyCharset), TTL)
          .handleErrorWith(L.error(_)(s"Error writing $VersionKey"))
          .whenA(fromRedis.isEmpty)
    yield version
  }

  /**
   * This method will get the version from the remote itc and compare it with the one on redis. If
   * there is none in redis we just store it If the remote is different than the local flush the
   * cache
   */
  def checkVersionToPurge[F[_]: MonadThrow: Logger](
    redis: StringCommands[F, Array[Byte], Array[Byte]] with Flush[F, Array[Byte]],
    itc:   Itc[F]
  ): F[Unit] = {
    val L      = Logger[F]
    val result = for
      _              <- L.info("Check for stale cache")
      remoteVersion  <- itc.itcVersions // Remote itc version
      _              <- L.info(s"Remote itc version $remoteVersion")
      fromRedis      <- redis.get(VersionKey)
      _              <- L.info(s"Local itc version $remoteVersion")
      versionOnRedis <- fromRedis.map(v => String(v, KeyCharset)).pure[F]
      // if the version changes flush redis
      _              <- (L.info("Flush redis cache on itc version change") *> redis.flushAll)
                          .whenA(versionOnRedis.exists(_ =!= remoteVersion))
      _              <- redis.setEx(VersionKey, remoteVersion.getBytes(KeyCharset), TTL)
    yield ()
    result.handleErrorWith(e => L.error(e)("Error doing version check to purge"))
  }
