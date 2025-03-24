// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.legacy

import algebra.instances.all.given
import cats.*
import cats.syntax.all.*
import coulomb.*
import coulomb.policy.spire.standard.given
import coulomb.syntax.*
import coulomb.units.si.*
import eu.timepit.refined.types.numeric.NonNegInt
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.data.Zipper
import lucuma.core.enums.Band
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.util.TimeSpan
import lucuma.itc.CalculationError
import lucuma.itc.IntegrationTime
import lucuma.itc.Millisecond
import lucuma.itc.SignalToNoiseAt
import lucuma.itc.TargetGraphsCalcResult
import lucuma.itc.TargetIntegrationTime
import lucuma.itc.legacy.codecs.given
import lucuma.itc.service.Itc
import lucuma.itc.service.ItcObservingConditions
import lucuma.itc.service.ObservingMode
import lucuma.itc.service.ObservingMode.ImagingMode
import lucuma.itc.service.ObservingMode.SpectroscopyMode
import lucuma.itc.service.TargetData
import natchez.Trace
import org.typelevel.log4cats.Logger

import scala.concurrent.duration.*
import scala.math.*

/** An ITC implementation that calls the OCS2 ITC server remotely. */
object ItcImpl {
  opaque type ExposureCount = Int

  def build[F[_]: MonadThrow: Logger: Trace](itcLocal: FLocalItc[F]): Itc[F] =
    new Itc[F] {
      val L = Logger[F]
      val T = Trace[F]

      private def fromLegacy(sn: lucuma.itc.legacy.SignalToNoiseAt): SignalToNoiseAt =
        SignalToNoiseAt(sn.wavelength, sn.single, sn.total)

      def calculateIntegrationTime(
        target:        TargetData,
        atWavelength:  Wavelength,
        observingMode: ObservingMode,
        constraints:   ItcObservingConditions,
        signalToNoise: SignalToNoise
      ): F[TargetIntegrationTime] =
        T.span("calculate-integration-time"):
          observingMode match
            case s @ (SpectroscopyMode.GmosNorth(_, _, _, _, _, _) |
                SpectroscopyMode.GmosSouth(_, _, _, _, _, _) |
                SpectroscopyMode.Flamingos2(_, _, _)) =>
              spectroscopyIntegrationTime(target, atWavelength, s, constraints, signalToNoise)
            case i @ (
                  ObservingMode.ImagingMode.GmosNorth(_, _) |
                  ObservingMode.ImagingMode.GmosSouth(_, _) |
                  ObservingMode.ImagingMode.Flamingos2(_)
                ) =>
              imagingIntegrationTime(target, atWavelength, i, constraints, signalToNoise)

      def calculateGraphs(
        target:        TargetData,
        atWavelength:  Wavelength,
        observingMode: ObservingMode,
        constraints:   ItcObservingConditions,
        exposureTime:  TimeSpan,
        exposureCount: NonNegInt
      ): F[TargetGraphsCalcResult] =
        observingMode match
          case s @ (SpectroscopyMode.GmosNorth(_, _, _, _, _, _) |
              SpectroscopyMode.GmosSouth(_, _, _, _, _, _) |
              SpectroscopyMode.Flamingos2(_, _, _)) =>
            spectroscopyGraphs(
              target,
              atWavelength,
              s,
              constraints,
              exposureTime.toMilliseconds.withUnit[Millisecond].toUnit[Second],
              exposureCount.value
            )
          case ImagingMode.GmosNorth(_, _) | ImagingMode.GmosSouth(_, _) |
              ImagingMode.Flamingos2(_) =>
            MonadThrow[F].raiseError:
              new IllegalArgumentException("Imaging mode not supported for graph calculation")

      private def spectroscopyGraphs(
        target:           TargetData,
        atWavelength:     Wavelength,
        observingMode:    ObservingMode,
        constraints:      ItcObservingConditions,
        exposureDuration: Quantity[BigDecimal, Second],
        exposureCount:    Int,
        level:            Option[NonNegInt] = none
      ): F[TargetGraphsCalcResult] =
        import lucuma.itc.legacy.*

        T.span("legacy-itc-query"):
          val (request, bandOrLine): (Json, Either[Band, Wavelength]) =
            spectroscopyGraphParams(
              target,
              atWavelength,
              observingMode,
              exposureDuration.value.toDouble.seconds,
              constraints,
              exposureCount
            ).leftMap(_.asJson)

          for
            _ <- T.put("itc.query" -> request.spaces2)
            _ <- T.put("itc.exposureDuration" -> exposureDuration.value.toInt)
            _ <- T.put("itc.exposures" -> exposureCount)
            _ <- T.put("itc.level" -> level.map(_.value).orEmpty)
            r <- itcLocal.calculateGraphs(request.noSpaces)
          yield TargetGraphsCalcResult.fromLegacy(r.ccds, r.groups, atWavelength, bandOrLine)

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
        import lucuma.itc.legacy.*

        val (request, bandOrLine): (Json, Either[Band, Wavelength]) =
          spectroscopyIntegrationTimeParams(
            target,
            atWavelength,
            observingMode,
            constraints,
            signalToNoise
          ).leftMap(_.asJson)

        for
          _ <- L.info(s"Desired S/N $signalToNoise")
          _ <- L.info(s"Target $target at wavelength $atWavelength")
          r <- T.span("itc.calctime.spectroscopy-integration-time"):
                 for
                   _      <- T.put("itc.query" -> request.spaces2)
                   _      <- T.put("itc.sigma" -> signalToNoise.toBigDecimal.toDouble)
                   _      <- L.info(request.noSpaces) // Request to the legacy itc
                   _      <- L.info(
                               s"Spectroscopy: Signal to noise mode ${request.noSpaces}"
                             ) // Request to the legacy itc
                   a      <- itcLocal.calculateIntegrationTime(request.noSpaces)
                   result <- convertIntegrationTimeRemoteResult(a, bandOrLine)
                 yield result
        yield r

      private def convertIntegrationTimeRemoteResult(
        r:          IntegrationTimeRemoteResult,
        bandOrLine: Either[Band, Wavelength]
      ): F[TargetIntegrationTime] =
        val tgts = r.exposureCalculation.exposures
          .traverse: r =>
            TimeSpan
              .fromSeconds(r.exposureTime)
              .map(expTime =>
                IntegrationTime(expTime, NonNegInt.unsafeFrom(r.exposureCount.value))
                  .pure[F]
              )
              .getOrElse:
                MonadThrow[F].raiseError:
                  CalculationError(s"Negative exposure time ${r.exposureTime}")
          .flatMap: ccdTimes =>
            Zipper
              .of(ccdTimes.head, ccdTimes.tail.toList*)
              .focusIndex(r.exposureCalculation.selectedIndex)
              .map(_.pure[F])
              .getOrElse:
                MonadThrow[F].raiseError:
                  CalculationError("Selected CCD index out of bounds")
        tgts.map(TargetIntegrationTime(_, bandOrLine, r.signalToNoiseAt.map(fromLegacy)))

      /**
       * Compute the exposure time and number of exposures required to achieve the desired
       * signal-to-noise under the requested conditions. Only for spectroscopy modes.
       */
      private def spectroscopySignalToNoise(
        target:        TargetData,
        atWavelength:  Wavelength,
        observingMode: ObservingMode.SpectroscopyMode,
        constraints:   ItcObservingConditions,
        exposureTime:  TimeSpan,
        exposureCount: NonNegInt
      ): F[TargetIntegrationTime] =
        import lucuma.itc.legacy.*

        val (request, bandOrLine): (Json, Either[Band, Wavelength]) =
          spectroscopySNParams(
            target,
            atWavelength,
            observingMode,
            constraints,
            exposureTime.toMilliseconds.toDouble.milliseconds,
            exposureCount.value
          ).leftMap(_.asJson)

        for
          _ <- L.info(s"Calculate S/N for exp time $exposureTime and count $exposureCount")
          _ <- L.info(s"Target $target at wavelength $atWavelength")
          r <- T.span("itc.calctime.spectroscopy-signal-to-noise"):
                 for
                   _ <- T.put("itc.query" -> request.spaces2)
                   _ <- L.info(
                          s"Spectroscopy: Signal to noise mode ${request.noSpaces}"
                        ) // Request to the legacy itc
                   a <- itcLocal.calculateSignalToNoise(request.noSpaces)
                 yield TargetIntegrationTime(
                   Zipper.one(IntegrationTime(exposureTime, exposureCount)),
                   bandOrLine,
                   a.signalToNoiseAt.map(fromLegacy)
                 )
        yield r

      def calculateSignalToNoise(
        target:        TargetData,
        atWavelength:  Wavelength,
        observingMode: ObservingMode,
        constraints:   ItcObservingConditions,
        exposureTime:  TimeSpan,
        exposureCount: NonNegInt
      ): F[TargetIntegrationTime] =
        T.span("calculate-signal-to-noise"):
          observingMode match
            case s @ (ObservingMode.SpectroscopyMode.GmosNorth(_, _, _, _, _, _) |
                ObservingMode.SpectroscopyMode.GmosSouth(_, _, _, _, _, _) |
                ObservingMode.SpectroscopyMode.Flamingos2(_, _, _)) =>
              spectroscopySignalToNoise(target,
                                        atWavelength,
                                        s,
                                        constraints,
                                        exposureTime,
                                        exposureCount
              )
            case s @ ObservingMode.ImagingMode.Flamingos2(_) =>
              imagingSignalToNoise(target,
                                   atWavelength,
                                   s,
                                   constraints,
                                   exposureTime,
                                   exposureCount
              )
            case u                                           =>
              MonadThrow[F].raiseError:
                CalculationError(s"Mode $u not supported for signal-to-noise calculation")

      /**
       * Compute the exposure time and number of exposures required to achieve the desired
       * signal-to-noise under the requested conditions. Only for spectroscopy modes
       */
      private def imagingIntegrationTime(
        target:        TargetData,
        atWavelength:  Wavelength,
        observingMode: ObservingMode.ImagingMode,
        constraints:   ItcObservingConditions,
        signalToNoise: SignalToNoise
      ): F[TargetIntegrationTime] =
        import lucuma.itc.legacy.*

        val (request, bandOrLine): (Json, Either[Band, Wavelength]) =
          imagingIntegrationTimeParams(target,
                                       atWavelength,
                                       observingMode,
                                       constraints,
                                       signalToNoise
          ).leftMap(
            _.asJson
          )

        for
          _ <- L.info(s"Desired S/N $signalToNoise")
          _ <- L.info(s"Target $target  at wavelength $atWavelength")
          r <- T.span("itc.calctime.spectroscopy-exp-time-at"):
                 for
                   _            <- T.put("itc.query" -> request.spaces2)
                   _            <- T.put("itc.sigma" -> signalToNoise.toBigDecimal.toDouble)
                   // Request to the legacy itc
                   _            <- L.info(s"Imaging: Signal to noise mode ${request.noSpaces}")
                   remoteResult <- itcLocal.calculateIntegrationTime(request.noSpaces)
                   result       <- convertIntegrationTimeRemoteResult(remoteResult, bandOrLine)
                 yield result
        yield r

      /**
       * Compute the exposure time and number of exposures required to achieve the desired
       * signal-to-noise under the requested conditions. Only for spectroscopy modes
       */
      private def imagingSignalToNoise(
        target:        TargetData,
        atWavelength:  Wavelength,
        observingMode: ObservingMode.ImagingMode,
        constraints:   ItcObservingConditions,
        exposureTime:  TimeSpan,
        exposureCount: NonNegInt
      ): F[TargetIntegrationTime] =
        import lucuma.itc.legacy.*

        val (request, bandOrLine): (Json, Either[Band, Wavelength]) =
          imagingS2NParams(
            target,
            atWavelength,
            observingMode,
            constraints,
            exposureTime.toMilliseconds.toDouble.milliseconds,
            exposureCount.value
          ).leftMap(_.asJson)

        for {
          _ <- L.info(s"Calculate S/N for exp time $exposureTime and count $exposureCount")
          _ <- L.info(s"Target $target  at wavelength $atWavelength")
          r <- T.span("itc.calctime.imaging-exp-time-at"):
                 for
                   _            <- T.put("itc.query" -> request.spaces2)
                   // Request to the legacy itc
                   _            <- L.info(s"Imaging: time and count mode ${request.noSpaces}")
                   remoteResult <- itcLocal.calculateIntegrationTime(request.noSpaces)
                   result       <- convertIntegrationTimeRemoteResult(remoteResult, bandOrLine)
                 yield result
        } yield r
    }

}
