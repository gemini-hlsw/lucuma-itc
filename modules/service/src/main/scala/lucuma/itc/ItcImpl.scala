// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import algebra.instances.all.given
import cats.*
import cats.data.NonEmptyChain
import cats.syntax.all.*
import coulomb.*
import coulomb.policy.spire.standard.given
import coulomb.syntax.*
import coulomb.units.si.*
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.Band
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.util.TimeSpan
import lucuma.itc.legacy.FLocalItc
import lucuma.itc.legacy.IntegrationTimeRemoteResult
import lucuma.itc.search.ObservingMode
import lucuma.itc.search.TargetData
import natchez.Trace
import org.typelevel.log4cats.Logger

import scala.concurrent.duration.*
import scala.math.*
import lucuma.core.data.Zipper

/** An ITC implementation that calls the OCS2 ITC server remotely. */
object ItcImpl {
  opaque type ExposureCount = Int

  def build[F[_]: MonadThrow: Logger: Trace](itcLocal: FLocalItc[F]): Itc[F] =
    new Itc[F] {
      val L = Logger[F]
      val T = Trace[F]

      def calculateIntegrationTime(
        target:        TargetData,
        atWavelength:  Wavelength,
        observingMode: ObservingMode,
        constraints:   ItcObservingConditions,
        signalToNoise: SignalToNoise
      ): F[TargetIntegrationTime] =
        T.span("calculate-integration-time"):
          observingMode match
            case s @ ObservingMode.SpectroscopyMode(_, _, _) =>
              spectroscopyIntegrationTime(target, atWavelength, s, constraints, signalToNoise)
            case i @ (
                  ObservingMode.ImagingMode.GmosNorth(_, _) |
                  ObservingMode.ImagingMode.GmosSouth(_, _)
                ) =>
              imaging(target, atWavelength, i, constraints, signalToNoise)

      def calculateGraph(
        target:        TargetData,
        atWavelength:  Wavelength,
        observingMode: ObservingMode,
        constraints:   ItcObservingConditions,
        exposureTime:  TimeSpan,
        exposureCount: PosInt
      ): F[TargetGraphsCalcResult] =
        observingMode match
          case s @ ObservingMode.SpectroscopyMode(_, _, _) =>
            spectroscopyGraph(
              target,
              atWavelength,
              s,
              constraints,
              exposureTime.toMilliseconds.withUnit[Millisecond].toUnit[Second],
              exposureCount.value
            )
          case ObservingMode.ImagingMode.GmosNorth(_, _) |
              ObservingMode.ImagingMode.GmosSouth(_, _) =>
            MonadThrow[F].raiseError:
              new IllegalArgumentException("Imaging mode not supported for graph calculation")

      private def itcGraph(
        target:           TargetData,
        band:             Band,
        observingMode:    ObservingMode,
        constraints:      ItcObservingConditions,
        exposureDuration: Quantity[BigDecimal, Second],
        exposureCount:    Int,
        level:            Option[NonNegInt] = none
      ): F[legacy.GraphsRemoteResult] =
        import lucuma.itc.legacy.given
        import lucuma.itc.legacy.*

        T.span("legacy-itc-query") {
          val request: Json =
            spectroscopyGraphParams(
              target,
              band,
              observingMode,
              exposureDuration.value.toDouble.seconds,
              constraints,
              exposureCount
            ).asJson

          for
            _ <- T.put("itc.query" -> request.spaces2)
            _ <- T.put("itc.exposureDuration" -> exposureDuration.value.toInt)
            _ <- T.put("itc.exposures" -> exposureCount)
            _ <- T.put("itc.level" -> level.map(_.value).orEmpty)
            r <- itcLocal.calculateGraphs(request.noSpaces)
          yield r
        }

      private def itcIntegrationTime(
        target:        TargetData,
        atWavelength:  Wavelength,
        observingMode: ObservingMode.SpectroscopyMode,
        constraints:   ItcObservingConditions,
        sigma:         SignalToNoise
      ): F[legacy.IntegrationTimeRemoteResult] =
        import lucuma.itc.legacy.given
        import lucuma.itc.legacy.*

        T.span("legacy-itc-query") {
          val (request, bandOrLine): (Json, Either[Band, Wavelength]) =
            spectroscopyExposureTimeParams(
              target,
              atWavelength,
              observingMode,
              constraints,
              sigma
            ).leftMap(_.asJson)

          for
            _ <- T.put("itc.query" -> request.spaces2)
            _ <- T.put("itc.sigma" -> sigma.toBigDecimal.toDouble)
            _ <- L.info(request.noSpaces) // Request to the legacy itc
            a <- itcLocal.calculateIntegrationTime(request.noSpaces)
          yield a
        }

      private def spectroscopyGraph(
        target:           TargetData,
        atWavelength:     Wavelength,
        band:             Band,
        observingMode:    ObservingMode.SpectroscopyMode,
        constraints:      ItcObservingConditions,
        exposureDuration: Quantity[BigDecimal, Second],
        exposureCount:    Int
      ): F[TargetGraphsCalcResult] =
        itcGraph(target, band, observingMode, constraints, exposureDuration, exposureCount).map:
          r => TargetGraphsCalcResult.fromLegacy(r.ccds, r.groups, atWavelength)

      /**
       * Compute the exposure time and number of exposures required to achieve the desired
       * signal-to-noise under the requested conditions. Only for spectroscopy modes.
       */
      private def spectroscopyIntegrationTime(
        target:        TargetData,
        atWavelength:  Wavelength,
        observingMode: ObservingMode.SpectroscopyMode,
        constraints:   ItcObservingConditions,
        signalToNoise: SignalToNoise
      ): F[TargetIntegrationTime] =
        for {
          _ <- L.info(s"Desired S/N $signalToNoise")
          _ <- L.info(s"Target $target at wavelength $atWavelength")
          r <-
            T.span("itc.calctime.spectroscopy-integration-time"):
              itcIntegrationTime(
                target,
                atWavelength,
                observingMode,
                constraints,
                signalToNoise
              )
          t <- convertIntegrationTimeRemoteResult(r)
        } yield t

      private def imagingLegacy(
        target:        TargetData,
        band:          Band,
        observingMode: ObservingMode.ImagingMode,
        constraints:   ItcObservingConditions,
        sigma:         SignalToNoise
      ): F[legacy.IntegrationTimeRemoteResult] =
        import lucuma.itc.legacy.given
        import lucuma.itc.legacy.*

        T.span("legacy-itc-query") {
          val request: Json =
            imagingParams(target, band, observingMode, constraints, sigma).asJson

          for
            _ <- T.put("itc.query" -> request.spaces2)
            _ <- T.put("itc.sigma" -> sigma.toBigDecimal.toDouble)
            _ <- L.info(request.noSpaces)
            r <- itcLocal.calculateIntegrationTime(request.noSpaces)
          yield r
        }

      private def convertIntegrationTimeRemoteResult(
        r:          IntegrationTimeRemoteResult,
        bandOrLine: Either[Band, Wavelength]
      ): F[TargetIntegrationTime] =
        r.exposureCalculation
          .traverse: r =>
            TimeSpan
              .fromSeconds(r.exposureTime)
              .map(expTime => IntegrationTime(expTime, r.exposureCount, r.signalToNoise).pure[F])
              .getOrElse:
                MonadThrow[F].raiseError:
                  CalculationError(s"Negative exposure time ${r.exposureTime}")
          .map: ccdTimes =>
            TargetIntegrationTime(Zipper.of(ccdTimes.head, ccdTimes.tail.toList*), bandOrLine)

      /**
       * Compute the exposure time and number of exposures required to achieve the desired
       * signal-to-noise under the requested conditions. Only for spectroscopy modes
       */
      private def imaging(
        target:        TargetData,
        band:          Band,
        observingMode: ObservingMode.ImagingMode,
        constraints:   ItcObservingConditions,
        signalToNoise: SignalToNoise
      ): F[NonEmptyChain[IntegrationTime]] =
        for {
          _ <- L.info(s"Desired S/N $signalToNoise")
          _ <- L.info(s"Target $target  at band $band")
          r <- T.span("itc.calctime.spectroscopy-exp-time-at"):
                 imagingLegacy(target, band, observingMode, constraints, signalToNoise)
          t <- convertIntegrationTimeRemoteResult(r)
        } yield t

    }
}
