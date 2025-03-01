// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
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
import lucuma.itc.service.redis.given
import lucuma.itc.service.requests.*
import natchez.Trace
import org.typelevel.log4cats.Logger

/**
 * Methods to check if a values is on the cache and if not retrieve them from old itc and store them
 * in the cache
 */
trait ItcCacheOrRemote extends Version:
  val VersionKey: String = "itc:version"

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
  def graphsFromCacheOrRemote[F[_]: MonadThrow: Logger: Trace: Clock](
    request: TargetGraphRequest
  )(
    itc:     Itc[F],
    cache:   BinaryEffectfulCache[F]
  ): F[TargetGraphsCalcResult] =
    cache.getOrInvokeBinary(request, requestGraphs(itc)(request), "itc:graph:spec")

  private def timeAndCountModeNotImplemented[F[_]: MonadThrow, A]: F[A] =
    MonadThrow[F].raiseError[A]:
      UnsupportedOperationException:
        "'Time And Count' exposure time mode is not yet implemented."

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
    cache:       BinaryEffectfulCache[F]
  ): F[TargetIntegrationTime] =
    cache.getOrInvokeBinary(calcRequest, requestSpecTimeCalc(itc)(calcRequest), "itc:calc:spec")

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
    cache:       BinaryEffectfulCache[F]
  ): F[TargetIntegrationTime] =
    cache.getOrInvokeBinary(calcRequest, requestImgTimeCalc(itc)(calcRequest), "itc:calc:img")

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
      _              <- (L.info( // if the version changes flush cache
                          s"Flush  cache on itc version change, set to ${BuildInfo.ocslibHash}"
                        ) *> cache.flush)
                          .whenA(versionOnCache.exists(_ =!= BuildInfo.ocslibHash))
      _              <- cache.writeBinary(VersionKey, BuildInfo.ocslibHash)
    yield ()
    result.handleErrorWith(e => L.error(e)("Error doing version check to purge"))
  }
