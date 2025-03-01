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
import lucuma.core.model.ExposureTimeMode
import lucuma.itc.*
import lucuma.itc.service.redis.given
import lucuma.itc.service.requests.*
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
  // Time to live for entries. The idea of having such a long TTL is that eviction is based on LRU
  // and cache size restriction. As such, Redis should be configured to use an LRU eviction policy.
  val TTL        = FiniteDuration(365, DAYS)
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
    val L           = Logger[F]

    val whenFound: F[Unit] =
      L.debug(s"$hash found on redis")

    val whenMissing: F[B] =
      for
        _ <- L.debug(s"$hash not found on redis")
        r <- Trace[F].span("request-call")(request(a))
        _ <-
          redis
            .setEx(redisKey, Pickle.intoBytes(r).compact().array(), TTL)
            .handleErrorWith(L.error(_)(s"Error writing $redisKey"))
      yield r

    Trace[F].span("redis-cache-read") {
      for
        _         <- Trace[F].put("redis.key" -> redisKeyStr)
        _         <- L.debug(s"Read key $redisKeyStr")
        fromRedis <-
          redis
            .get(redisKey)
            .handleErrorWith(e => L.error(e)(s"Error reading $redisKey") *> none.pure[F])
            .map:
              _.flatMap: b =>
                Either.catchNonFatal(Unpickle[B].fromBytes(ByteBuffer.wrap(b))).toOption
        r         <- fromRedis.fold(whenMissing)(whenFound.as(_))
      yield r
    }
  }

  private def requestGraphs[F[_]: Functor](itc: Itc[F])(
    request: TargetGraphRequest
  ): F[TargetGraphsCalcResult] =
    itc
      .calculateGraphs(
        request.target,
        request.atWavelength,
        request.specMode,
        request.constraints,
        request.expTime,
        request.exp
      )

  /**
   * Request a graph
   */
  def graphsFromCacheOrRemote[F[_]: MonadThrow: Logger: Trace: Clock](
    request: TargetGraphRequest
  )(
    itc:     Itc[F],
    redis:   StringCommands[F, Array[Byte], Array[Byte]]
  ): F[TargetGraphsCalcResult] =
    cacheOrRemote(request, requestGraphs(itc))("itc:graph:spec", redis)

  private def timeAndCountModeNotImplemented[F[_]: MonadThrow, A]: F[A] =
    MonadThrow[F].raiseError[A](
      new UnsupportedOperationException(
        "'Time And Count' exposure time mode is not yet implemented."
      )
    )

  private def requestSpecTimeCalc[F[_]: MonadThrow](itc: Itc[F])(
    calcRequest: TargetSpectroscopyTimeRequest
  ): F[TargetIntegrationTime] =
    calcRequest.exposureTimeMode match
      case ExposureTimeMode.SignalToNoiseMode(value, at) =>
        itc
          .calculateIntegrationTime(
            calcRequest.target,
            at,
            calcRequest.specMode,
            calcRequest.constraints,
            value
          )
      case ExposureTimeMode.TimeAndCountMode(_, _, _)    =>
        timeAndCountModeNotImplemented

  /**
   * Request exposure time calculation for spectroscopy
   */
  def specTimeFromCacheOrRemote[F[_]: MonadThrow: Logger: Trace: Clock](
    calcRequest: TargetSpectroscopyTimeRequest
  )(
    itc:         Itc[F],
    redis:       StringCommands[F, Array[Byte], Array[Byte]]
  ): F[TargetIntegrationTime] =
    cacheOrRemote(calcRequest, requestSpecTimeCalc(itc))(
      "itc:calc:spec",
      redis
    )

  private def requestImgTimeCalc[F[_]: MonadThrow](itc: Itc[F])(
    calcRequest: TargetImagingTimeRequest
  ): F[TargetIntegrationTime] =
    calcRequest.exposureTimeMode match
      case ExposureTimeMode.SignalToNoiseMode(value, at) =>
        itc
          .calculateIntegrationTime(
            calcRequest.target,
            at,
            calcRequest.imagingMode,
            calcRequest.constraints,
            value
          )
      case ExposureTimeMode.TimeAndCountMode(_, _, _)    =>
        timeAndCountModeNotImplemented

  /**
   * Request exposure time calculation for imaging
   */
  def imgTimeFromCacheOrRemote[F[_]: MonadThrow: Logger: Trace: Clock](
    calcRequest: TargetImagingTimeRequest
  )(
    itc:         Itc[F],
    redis:       StringCommands[F, Array[Byte], Array[Byte]]
  ): F[TargetIntegrationTime] =
    cacheOrRemote(calcRequest, requestImgTimeCalc(itc))(
      "itc:calc:img",
      redis
    )

  /**
   * This method will get the version from the remote itc and compare it with the one on redis. If
   * there is none in redis we just store it If the remote is different than the local flush the
   * cache.
   */
  def checkVersionToPurge[F[_]: MonadThrow: Logger](
    redis: StringCommands[F, Array[Byte], Array[Byte]] & Flush[F, Array[Byte]],
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
