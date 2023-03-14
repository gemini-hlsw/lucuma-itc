// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import boopickle.DefaultBasic.*
import buildinfo.BuildInfo
import cats.*
import cats.effect.kernel.Clock
import cats.syntax.all.*
import dev.profunktor.redis4cats.algebra.Flush
import dev.profunktor.redis4cats.algebra.StringCommands
import lucuma.itc.*
import lucuma.itc.search.ItcVersions
import lucuma.itc.service.config.ExecutionEnvironment
import lucuma.itc.service.redis.given
import natchez.Trace
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
  private def cacheOrRemote[F[_]: MonadThrow: Logger: Trace: Clock, A: Hash, B: Pickler](
    a:       A,
    request: A => F[B]
  )(
    prefix:  String,
    redis:   StringCommands[F, Array[Byte], Array[Byte]]
  ): F[B] = {
    val hash        = Hash[A].hash(a)
    val redisKeyStr = s"$prefix:$hash"
    val redisKey    = redisKeyStr.getBytes(KeyCharset)
    val redisKey2   = s"$redisKeyStr 1".getBytes(KeyCharset)
    val L           = Logger[F]

    Trace[F].span("redis-cache-read") {
      for
        _         <- Trace[F].put("redis.key" -> redisKeyStr)
        _         <- L.info(s"Read key $redisKeyStr")
        fromRedis <- redis
                       .get(redisKey2)
                       .handleErrorWith(e => L.error(e)(s"Error reading $redisKey") *> none.pure[F])
        decoded   <-
          fromRedis
            .flatMap(b => Either.catchNonFatal(Unpickle[B].fromBytes(ByteBuffer.wrap(b))).toOption)
            .pure[F]
        _         <- L.info(s"$hash found on redis").unlessA(fromRedis.isEmpty && decoded.isEmpty)
        r         <- decoded.map(_.pure[F]).getOrElse(Trace[F].span("request-call")(request(a)))
        _         <-
          redis
            .setEx(redisKey, Pickle.intoBytes(r).compact().array(), TTL)
            .handleErrorWith(L.error(_)(s"Error writing $redisKey"))
            .whenA(fromRedis.isEmpty || decoded.isEmpty)
      yield r
    }
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
  def graphFromCacheOrRemote[F[_]: MonadThrow: Logger: Trace: Clock](
    request: GraphRequest
  )(itc:     Itc[F], redis: StringCommands[F, Array[Byte], Array[Byte]]): F[GraphResult] =
    cacheOrRemote(request, requestGraph(itc))("itc:graph:spec", redis)

  private val requestCalc = [F[_]] =>
    (itc: Itc[F]) =>
      (calcRequest: CalcRequest) =>
        itc
          .calculateExposureTime(
            calcRequest.targetProfile,
            calcRequest.specMode,
            calcRequest.constraints,
            calcRequest.signalToNoise.value,
            calcRequest.signalToNoiseAt
        )

  /**
   * Request exposure time calculation
   */
  def calcFromCacheOrRemote[F[_]: MonadThrow: Logger: Trace: Clock](
    calcRequest: CalcRequest
  )(
    itc:         Itc[F],
    redis:       StringCommands[F, Array[Byte], Array[Byte]]
  ): F[ExposureCalculationResult] =
    Logger[F].info(calcRequest.toString) *> cacheOrRemote(calcRequest, requestCalc(itc))(
      "itc:calc:spec",
      redis
    )

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
      _              <- L.info(s"Current itc data checksum ${BuildInfo.ocslibHash}")
      fromRedis      <- redis.get(VersionKey)
      versionOnRedis <- fromRedis.map(v => String(v, KeyCharset)).pure[F]
      _              <- L.info(s"itc data checksum on redis $versionOnRedis")
      // if the version changes flush redis
      _              <- (L.info(
                          s"Flush redis cache on itc version change, set to ${BuildInfo.ocslibHash}"
                        ) *> redis.flushAll)
                          .whenA(versionOnRedis.exists(_ =!= BuildInfo.ocslibHash))
      _              <- redis.setEx(VersionKey, BuildInfo.ocslibHash.getBytes(KeyCharset), TTL)
    yield ()
    result.handleErrorWith(e => L.error(e)("Error doing version check to purge"))
  }
