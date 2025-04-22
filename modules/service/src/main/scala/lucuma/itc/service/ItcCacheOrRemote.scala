// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import boopickle.DefaultBasic.*
import buildinfo.BuildInfo
import cats.*
import cats.effect.kernel.Clock
import cats.syntax.all.*
import lucuma.core.model.ExposureTimeMode
import lucuma.itc.*
import lucuma.itc.cache.BinaryEffectfulCache
import lucuma.itc.input.customSed.CustomSed
import lucuma.itc.service.redis.given
import lucuma.itc.service.requests.*
import natchez.Trace
import org.typelevel.log4cats.Logger

import scala.concurrent.duration.*

/**
 * Methods to check if a values is on the cache and if not retrieve them from old itc and store them
 * in the cache
 */
trait ItcCacheOrRemote extends Version:
  val VersionKey: String                  = "itc:version"
  // Time to live for entries. The idea of having such a long TTL is that eviction is based on LRU
  // and cache size restriction. As such, Redis should be configured to use the `volatile-lru`
  // eviction policy. This will work better than `allkeys-lru` since it will prevent the version
  // key from being evicted, which will cause the cache to flush.
  private val TTL: Option[FiniteDuration] = FiniteDuration(365, DAYS).some

  private def requestGraphs[F[_]: Functor](
    itc: Itc[F]
  )(request: TargetGraphRequest): F[TargetGraphsCalcResult] =
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
  def graphsFromCacheOrRemote[F[_]: MonadThrow: Parallel: Logger: Trace: Clock: CustomSed.Resolver](
    request: TargetGraphRequest
  )(
    itc:     Itc[F],
    cache:   BinaryEffectfulCache[F]
  ): F[TargetGraphsCalcResult] =
    CustomSed // We must resolve CustomSed before caching.
      .resolveTargetGraphRequest(request)
      .flatMap: r =>
        cache.getOrInvokeBinary(r, requestGraphs(itc)(r), TTL, "itc:graph:spec")

  private def requestSpecSNCalc[F[_]: MonadThrow](itc: Itc[F])(
    calcRequest: TargetSpectroscopyTimeRequest,
    mode:        ExposureTimeMode.TimeAndCountMode
  ): F[TargetIntegrationTime] =
    itc
      .calculateSignalToNoise(
        calcRequest.target,
        mode.at,
        calcRequest.specMode,
        calcRequest.constraints,
        mode.time,
        mode.count
      )

  private def requestSpecTimeCalc[F[_]: MonadThrow](itc: Itc[F])(
    calcRequest: TargetSpectroscopyTimeRequest,
    mode:        ExposureTimeMode.SignalToNoiseMode
  ): F[TargetIntegrationTime] =
    itc
      .calculateIntegrationTime(
        calcRequest.target,
        mode.at,
        calcRequest.specMode,
        calcRequest.constraints,
        mode.value
      )

  /**
   * Request exposure time calculation for spectroscopy
   */
  def spectroscopyFromCacheOrRemote[F[
    _
  ]: MonadThrow: Parallel: Logger: Trace: Clock: CustomSed.Resolver](
    calcRequest: TargetSpectroscopyTimeRequest
  )(
    itc:         Itc[F],
    cache:       BinaryEffectfulCache[F]
  ): F[TargetIntegrationTime] =
    CustomSed // We must resolve CustomSed before caching.
      .resolveTargetSpectroscopyTimeRequest(calcRequest)
      .flatMap: r =>
        r.exposureTimeMode match
          case m @ ExposureTimeMode.SignalToNoiseMode(_, _)   =>
            cache.getOrInvokeBinary(r, requestSpecTimeCalc(itc)(r, m), TTL, "itc:calc:spec:sn")
          case m @ ExposureTimeMode.TimeAndCountMode(_, _, _) =>
            cache.getOrInvokeBinary(r, requestSpecSNCalc(itc)(r, m), TTL, "itc:calc:spec:tc")

  private def requestImgTimeCalc[F[_]: MonadThrow](itc: Itc[F])(
    calcRequest: TargetImagingTimeRequest,
    mode:        ExposureTimeMode.SignalToNoiseMode
  ): F[TargetIntegrationTime] =
    itc
      .calculateIntegrationTime(
        calcRequest.target,
        mode.at,
        calcRequest.imagingMode,
        calcRequest.constraints,
        mode.value
      )

  private def requestImgSNCalc[F[_]: MonadThrow](itc: Itc[F])(
    calcRequest: TargetImagingTimeRequest,
    mode:        ExposureTimeMode.TimeAndCountMode
  ): F[TargetIntegrationTime] =
    itc
      .calculateSignalToNoise(
        calcRequest.target,
        mode.at,
        calcRequest.imagingMode,
        calcRequest.constraints,
        mode.time,
        mode.count
      )

  /**
   * Request exposure time calculation for imaging
   */
  def imagingFromCacheOrRemote[
    F[_]: MonadThrow: Parallel: Logger: Trace: Clock: CustomSed.Resolver
  ](
    calcRequest: TargetImagingTimeRequest
  )(
    itc:         Itc[F],
    cache:       BinaryEffectfulCache[F]
  ): F[TargetIntegrationTime] =
    CustomSed // We must resolve CustomSed before caching.
      .resolveTargetImagingTimeRequest(calcRequest)
      .flatMap: r =>
        r.exposureTimeMode match
          case m @ ExposureTimeMode.SignalToNoiseMode(_, _)   =>
            cache.getOrInvokeBinary(r, requestImgTimeCalc(itc)(r, m), TTL, "itc:calc:img:sn")
          case m @ ExposureTimeMode.TimeAndCountMode(_, _, _) =>
            cache.getOrInvokeBinary(r, requestImgSNCalc(itc)(r, m), TTL, "itc:calc:img:tc")

  /**
   * This method will get the version from the remote itc and compare it with the one on the cache.
   * If there is none in the cache we just store it. If the remote is different than the local then
   * flush the cache.
   */
  def checkVersionToPurge[F[_]: MonadThrow: Logger](
    cache: BinaryEffectfulCache[F],
    itc:   Itc[F]
  ): F[Unit] = {
    val L      = Logger[F]
    val result = for
      _              <- L.info("Check for stale cache")
      _              <- L.info(s"Current itc data checksum ${BuildInfo.ocslibHash}")
      versionOnCache <- cache.readBinary[String, String](VersionKey)
      _              <- L.info(s"itc data checksum on cache $versionOnCache")
      _              <-
        (L.info( // if the version changes or is missing, flush cache
          s"Flush cache on missing or changed ITC library version, set to [${BuildInfo.ocslibHash}]"
        ) *> cache.flush)
          .whenA(versionOnCache.forall(_ =!= BuildInfo.ocslibHash))
      _              <- cache.writeBinary(VersionKey, BuildInfo.ocslibHash, none)
    yield ()
    result.handleErrorWith(e => L.error(e)("Error doing version check to purge"))
  }
