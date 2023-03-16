// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import algebra.instances.all.given
import cats.*
import cats.data.NonEmptyList
import cats.effect.*
import cats.syntax.all.*
import coulomb.*
import coulomb.ops.algebra.spire.all.given
import coulomb.policy.spire.standard.given
import coulomb.syntax.*
import coulomb.units.si.*
import coulomb.units.si.given
import eu.timepit.refined.refineV
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.numeric.PosLong
import io.circe.Decoder
import io.circe.syntax.*
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.core.model.NonNegDuration
import lucuma.itc.Itc
import lucuma.itc.legacy.LocalItc
import lucuma.itc.search.ObservingMode
import lucuma.itc.search.TargetProfile
import lucuma.refined.*
import natchez.Trace
import natchez.http4s.NatchezMiddleware
import org.http4s.*
import org.http4s.circe.*
import org.http4s.dsl.io.*
import org.http4s.syntax.all.*
import org.typelevel.log4cats.Logger

import java.time.Duration
import scala.concurrent.duration.*
import scala.math.*

trait SignalToNoiseCalculation[F[_]: cats.Applicative] { this: Itc[F] =>
  def calculateSignalToNoise(
    graph:           NonEmptyList[ItcChartGroup],
    signalToNoiseAt: Option[Wavelength]
  ): F[SNCalcResult] =
    (for {
      s2nChart     <- graph.flatMap(_.charts).find(_.chartType === ChartType.S2NChart)
      finalS2NData <- s2nChart.series
                        .filter(_.seriesType === SeriesDataType.FinalS2NData)
                        .map(_.data)
                        .flatten
                        .some
    } yield {
      val sorted           = finalS2NData.sortBy(_._1)
      val sn: SNCalcResult = signalToNoiseAt
        .fold(SNCalcResult.SNCalcSuccess(sorted.maxBy(_._2)._2)) { w =>
          val nanos = Wavelength.decimalNanometers.reverseGet(w).doubleValue
          if (nanos < sorted.head._1) SNCalcResult.WavelengthAtBelowRange(w)
          else if (nanos > sorted.last._1) SNCalcResult.WavelengthAtAboveRange(w)
          else
            val sortedList = sorted.toList
            val index      = sortedList.indexWhere(_._1 >= nanos)
            sortedList.lift(index).fold(SNCalcResult.NoData()) { secondPoint =>
              val (w2, s2) = secondPoint
              if (w2 === nanos) {
                SNCalcResult.SNCalcSuccess(s2)
              } else {
                sortedList.lift(index - 1) match
                  case Some((w1, s1)) =>
                    // Linear interpolation
                    val sn = (s1 * (w2 - nanos) + s2 * (nanos - w1)) / (w2 - w1)
                    SNCalcResult.SNCalcSuccess(sn)
                  case _              =>
                    // We are checking the limits before, this shouldn't happen
                    SNCalcResult.NoData()
              }
            }
        }
      sn.pure[F]
    }).getOrElse(SNCalcResult.NoData().pure[F])

}

/** An ITC implementation that calls the OCS2 ITC server remotely. */
object ItcImpl {
  opaque type NumberOfExposures = Int

  val Error400Regex = "<title>Error 400 (.*)</title>".r

  def build[F[_]: MonadThrow: Logger: Trace](itcLocal: LocalItc, dataVersion: String): Itc[F] =
    new Itc[F] with SignalToNoiseCalculation[F] {
      val L = Logger[F]

      def calculateExposureTime(
        targetProfile:   TargetProfile,
        observingMode:   ObservingMode,
        constraints:     ItcObservingConditions,
        signalToNoise:   BigDecimal,
        signalToNoiseAt: Option[Wavelength]
      ): F[ExposureTimeResult] =
        Trace[F].span("calculate-exposure-time") {
          observingMode match
            case _: ObservingMode.Spectroscopy =>
              signalToNoiseAt match
                case None     =>
                  spectroscopy(targetProfile, observingMode, constraints, signalToNoise, none)
                case Some(at) =>
                  spectroscopySNAt(targetProfile, observingMode, constraints, signalToNoise, at)
            // TODO: imaging
        }

      def calculateGraph(
        targetProfile: TargetProfile,
        observingMode: ObservingMode,
        constraints:   ItcObservingConditions,
        exposureTime:  NonNegDuration,
        exposures:     PosLong
      ): F[GraphResult] =
        observingMode match
          case _: ObservingMode.Spectroscopy =>
            spectroscopyGraph(
              targetProfile,
              observingMode,
              constraints,
              BigDecimal(exposureTime.value.toMillis).withUnit[Millisecond].toUnit[Second],
              exposures.value
            )
          // TODO: imaging

      // Convenience method to compute an OCS2 ITC result for the specified profile/mode.
      def itc(
        targetProfile:    TargetProfile,
        observingMode:    ObservingMode,
        constraints:      ItcObservingConditions,
        exposureDuration: Quantity[BigDecimal, Second],
        exposures:        Int,
        level:            NonNegInt
      ): F[legacy.GraphsRemoteResult] =
        import lucuma.itc.legacy.given
        import lucuma.itc.legacy.*

        Trace[F].span("legacy-itc-query") {
          val request =
            spectroscopyParams(targetProfile,
                               observingMode,
                               exposureDuration.value.toDouble.seconds,
                               constraints,
                               exposures
            ).asJson
          Trace[F].put("itc.query"              -> request.spaces2) *>
            Trace[F].put("itc.exposureDuration" -> exposureDuration.value.toInt) *>
            Trace[F].put("itc.exposures" -> exposures) *>
            Trace[F].put("itc.level" -> level.value) *>
            (itcLocal.calculateCharts(request.noSpaces) match {
              case Right(r)  => r.pure[F]
              case Left(msg) =>
                L.warn(s"Upstream error $msg") *>
                  ApplicativeThrow[F].raiseError(new UpstreamException(msg))
            })
        }

      def itcWithSNAt(
        targetProfile: TargetProfile,
        observingMode: ObservingMode,
        constraints:   ItcObservingConditions,
        sigma:         BigDecimal,
        wavelength:    Wavelength
      ): F[legacy.ExposureTimeRemoteResult] =
        import lucuma.itc.legacy.given
        import lucuma.itc.legacy.*

        Trace[F].span("legacy-itc-query") {
          val request =
            spectroscopyWithSNAtParams(targetProfile,
                                       observingMode,
                                       constraints,
                                       sigma,
                                       wavelength
            ).asJson
          Trace[F].put("itc.query"   -> request.spaces2) *>
            Trace[F].put("itc.sigma" -> sigma.toDouble) *>
            Logger[F].info(request.noSpaces) *>
            (itcLocal.calculateExposureTime(request.noSpaces) match {
              case Right(r)  => r.pure[F]
              case Left(msg) =>
                L.warn(s"Upstream 1 error $msg") *>
                  ApplicativeThrow[F].raiseError(new UpstreamException(msg))
            })
        }

      def itcGraph(
        targetProfile:    TargetProfile,
        observingMode:    ObservingMode,
        constraints:      ItcObservingConditions,
        exposureDuration: Quantity[BigDecimal, Second],
        exposures:        Long
      ): F[legacy.GraphsRemoteResult] =
        import lucuma.itc.legacy.given
        import lucuma.itc.legacy.*

        Trace[F].span("legacy-itc-query") {
          val json =
            spectroscopyParams(targetProfile,
                               observingMode,
                               exposureDuration.value.toDouble.seconds,
                               constraints,
                               exposures.toInt
            ).asJson
          Trace[F].put("itc.query"              -> json.spaces2) *>
            Trace[F].put("itc.exposureDuration" -> exposureDuration.value.toInt) *>
            Trace[F].put("itc.exposures" -> exposures.toInt) *>
            (itcLocal.calculateCharts(json.noSpaces) match {
              case Right(r)  => r.pure[F]
              case Left(msg) =>
                L.warn(s"Upstream error $msg") *>
                  ApplicativeThrow[F].raiseError(new UpstreamException(msg))
            })
        }

      val MaxIterations = 10

      def spectroscopyGraph(
        targetProfile:    TargetProfile,
        observingMode:    ObservingMode,
        constraints:      ItcObservingConditions,
        exposureDuration: Quantity[BigDecimal, Second],
        exposures:        Long
      ): F[GraphResult] =
        itcGraph(targetProfile, observingMode, constraints, exposureDuration, exposures).map { r =>
          GraphResult.fromLegacy(dataVersion, r.ccds, r.groups)
        }

      /**
       * Compute the exposure time and number of exposures required to achieve the desired
       * signal-to-noise under the requested conditions. Only for spectroscopy modes
       */
      def spectroscopy(
        targetProfile:   TargetProfile,
        observingMode:   ObservingMode,
        constraints:     ItcObservingConditions,
        signalToNoise:   BigDecimal,
        signalToNoiseAt: Option[Wavelength]
      ): F[ExposureTimeResult] = {
        val startExpTime      = BigDecimal(1200.0).withUnit[Second]
        val numberOfExposures = 1
        val requestedSN       = signalToNoise.toDouble

        // This loops should be necessary only a few times but put a circuit breaker just in case
        def itcStep(
          nExp:       NumberOfExposures,
          oldNExp:    Int,
          expTime:    Quantity[BigDecimal, Second],
          oldExpTime: Quantity[BigDecimal, Second],
          snr:        Double,
          maxTime:    Quantity[BigDecimal, Second],
          s:          legacy.GraphsRemoteResult,
          counter:    NonNegInt
        ): F[ExposureTimeResult] =
          if (snr === 0.0) {
            ApplicativeThrow[F].raiseError(new ItcCalculationError("S/N obtained is 0"))
          } else {
            val totalTime: Quantity[BigDecimal, Second] =
              expTime * nExp.withUnit[1] * pow(requestedSN / snr, 2).withUnit[1]

            val newNExp: BigDecimal = spire.math.ceil((totalTime / maxTime).value)

            val newExpTime: BigDecimal =
              spire.math.ceil((totalTime / newNExp.withUnit[1]).value)

            val next = NonNegInt.from(counter.value + 1).getOrElse(sys.error("Should not happen"))
            L.info(s"Total time: $totalTime maxTime: $maxTime") *>
              L.info(s"Exp time :$newExpTime s/Num exp $newNExp/iteration $counter") *> {
                if (
                  nExp != oldNExp ||
                  ((expTime - oldExpTime) > 1.withUnit[Second] || (oldExpTime - expTime) > 1
                    .withUnit[Second]) &&
                  counter.value < MaxIterations &&
                  newExpTime < (pow(2, 63) - 1)
                ) {
                  itc(targetProfile,
                      observingMode,
                      constraints,
                      newExpTime.withUnit[Second],
                      newNExp.toInt,
                      next
                  )
                    .flatMap { s =>
                      L.debug(s"-> S/N: ${s.maxTotalSNRatio}") *>
                        calculateSignalToNoise(s.groups, signalToNoiseAt).flatMap {
                          case SNCalcResult.SNCalcSuccess(snr) =>
                            itcStep(newNExp.toInt,
                                    nExp,
                                    newExpTime.withUnit[Second],
                                    expTime,
                                    snr.toDouble,
                                    maxTime,
                                    s,
                                    next
                            )
                          case r                               =>
                            ExposureTimeResult.CalculationError(r.toString).pure[F]
                        }
                    }
                } else
                  (refineV[Positive](newNExp.toInt).toOption match
                    case Some(count) =>
                      ExposureTimeResult
                        .ExposureTimeSuccess(newExpTime.toDouble.seconds, count, s.maxTotalSNRatio)
                    case _           => ExposureTimeResult.CalculationError("Negative exposure count")
                  )
                    .pure[F]
                    .widen[ExposureTimeResult]
              }
          }

        L.info(s"Desired S/N $signalToNoise") *>
          L.info(
            s"Target brightness ${targetProfile} at band ${targetProfile.band}"
          ) *>
          Trace[F]
            .span("itc.calctime.spectroscopy") {

              itc(targetProfile,
                  observingMode,
                  constraints,
                  startExpTime,
                  numberOfExposures,
                  1.refined
              )
                .flatMap { r =>
                  val halfWellTime = r.maxWellDepth / 2 / r.maxPeakPixelFlux * startExpTime.value
                  L.info(
                    s"Results CCD wellDepth: ${r.maxWellDepth}, peakPixelFlux: ${r.maxPeakPixelFlux}, totalSNRatio: ${r.maxTotalSNRatio} $halfWellTime"
                  ) *> {
                    if (halfWellTime < 1.0) {
                      val msg = s"Target is too bright. Well half filled in $halfWellTime"
                      L.error(msg) *>
                        ExposureTimeResult
                          .SourceTooBright(halfWellTime)
                          .pure[F]
                          .widen
                    } else {
                      val maxTime = startExpTime.value.min(halfWellTime)
                      calculateSignalToNoise(r.groups, signalToNoiseAt)
                        .flatMap {
                          case SNCalcResult.SNCalcSuccess(snr)        =>
                            itcStep(numberOfExposures,
                                    0,
                                    startExpTime,
                                    BigDecimal(0).withUnit[Second],
                                    snr.toDouble,
                                    maxTime.withUnit[Second],
                                    r,
                                    0.refined
                            )
                          case SNCalcResult.WavelengthAtAboveRange(w) =>
                            ExposureTimeResult
                              .CalculationError(
                                f"S/N at ${Wavelength.decimalNanometers.reverseGet(w)}%.0f nm above range"
                              )
                              .pure[F]
                          case SNCalcResult.WavelengthAtBelowRange(w) =>
                            ExposureTimeResult
                              .CalculationError(
                                f"S/N at ${Wavelength.decimalNanometers.reverseGet(w)}%.0f nm below range"
                              )
                              .pure[F]
                          case r                                      =>
                            ExposureTimeResult.CalculationError(r.toString).pure[F]
                        }
                    }
                  }

                }
            }

      }

      /**
       * Compute the exposure time and number of exposures required to achieve the desired
       * signal-to-noise under the requested conditions. Only for spectroscopy modes
       */
      def spectroscopySNAt(
        targetProfile:   TargetProfile,
        observingMode:   ObservingMode,
        constraints:     ItcObservingConditions,
        signalToNoise:   BigDecimal,
        signalToNoiseAt: Wavelength
      ): F[ExposureTimeResult] = {
        val startExpTime      = BigDecimal(1200.0).withUnit[Second]
        val numberOfExposures = 1
        val requestedSN       = signalToNoise.toDouble

        L.info(s"Desired S/N $signalToNoise") *>
          L.info(
            s"Target brightness ${targetProfile} at band ${targetProfile.band}"
          ) *>
          Trace[F]
            .span("itc.calctime.spectroscopy-exp-time-at") {
              itcWithSNAt(targetProfile, observingMode, constraints, signalToNoise, signalToNoiseAt)
                .flatMap { r =>
                  (refineV[Positive](r.exposureCalculation.exposures).toOption match
                    case Some(exposures) =>
                      ExposureTimeResult
                        .ExposureTimeSuccess(
                          r.exposureCalculation.exposureTime.seconds,
                          exposures,
                          r.exposureCalculation.signalToNoise
                        )
                    case _               => ExposureTimeResult.CalculationError("Negative exposure count")
                  ).pure[F]
                }
            }

      }
    }

}
