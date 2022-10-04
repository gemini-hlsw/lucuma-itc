// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import cats.data.NonEmptyList
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosLong
import io.circe.*
import io.circe.syntax.*
import lucuma.core.math.Wavelength
import lucuma.core.model.NonNegDuration
import lucuma.core.util.Enumerated
import lucuma.itc.encoders.given
import lucuma.itc.legacy.ItcRemoteCcd
import lucuma.itc.search.*

import scala.concurrent.duration.FiniteDuration

final case class UpstreamException(msg: String) extends RuntimeException(msg)

trait Itc[F[_]]:

  /**
   * Compute the exposure time and number required to achieve the desired signal-to-noise under
   * average conditions.
   */
  def calculateExposureTime(
    targetProfile: TargetProfile,
    observingMode: ObservingMode,
    constraints:   ItcObservingConditions,
    signalToNoise: BigDecimal,
    atWavelength:  Option[Wavelength]
  ): F[Itc.CalcResultWithVersion]

  /**
   * Retrieve the graph data for the given mode and exposureTime and exposures
   */
  def calculateGraph(
    targetProfile: TargetProfile,
    observingMode: ObservingMode,
    constraints:   ItcObservingConditions,
    exposureTime:  NonNegDuration,
    exposures:     PosLong
  ): F[Itc.GraphResult]

  /**
   * Calculate the signal to noise from graph data for the given mode and exposureTime and exposures
   */
  def calculateSignalToNoise(
    graph:        NonEmptyList[ItcChartGroup],
    atWavelength: Option[Wavelength]
  ): F[Itc.SNCalcResult]

  def itcVersions: F[String]

object Itc:

  def apply[F[_]](using ev: Itc[F]): ev.type = ev

  sealed trait CalcResult extends Product with Serializable {}
  object CalcResult:

    case class Success(
      exposureTime:  FiniteDuration,
      exposures:     Int,
      signalToNoise: BigDecimal
    ) extends CalcResult
        derives Encoder.AsObject

    /** Object is too bright to be observed in the specified mode. */
    case class SourceTooBright(msg: String) extends CalcResult

    /** Generic calculation error */
    case class CalculationError(msg: String) extends CalcResult

    given Encoder[Itc.CalcResult] = Encoder.instance {
      case f: Itc.CalcResult.Success          =>
        Json.obj(("resultType", Json.fromString("Success"))).deepMerge(f.asJson)
      case Itc.CalcResult.CalculationError(m) =>
        Json.obj(("resultType", Json.fromString("Error")), ("msg", Json.fromString(m)))
      case Itc.CalcResult.SourceTooBright(m)  =>
        Json.obj(("resultType", Json.fromString("Error")),
                 ("msg", Json.fromString(s"Source too bright $m"))
        )
    }

  case class CalcResultWithVersion(result: CalcResult, dataVersion: Option[String] = None)

  case class GraphResult(
    dataVersion: String,
    ccds:        NonEmptyList[ItcCcd],
    charts:      NonEmptyList[ItcChartGroup]
  )

  object GraphResult:
    def fromLegacy(
      dataVersion: String,
      ccds:        NonEmptyList[ItcRemoteCcd],
      charts:      NonEmptyList[ItcChartGroup]
    ): GraphResult = {
      def maxWavelength(chart: ItcChart, seriesDataType: SeriesDataType): List[Wavelength] =
        chart.series
          .filter(_.seriesType === seriesDataType)
          .zip(ccds.toList)
          .map { case (sn, ccd) =>
            sn.data
              .maxByOption(_._2)
              .map(_._1)
              .flatMap(i => Wavelength.fromPicometers.getOption((i * 1000).toInt))
          }
          .flattenOption

      def maxValue(chart: ItcChart, seriesDataType: SeriesDataType): List[Double] =
        chart.series
          .filter(_.seriesType === seriesDataType)
          .zip(ccds.toList)
          .map { case (sn, ccd) =>
            sn.data
              .maxByOption(_._2)
              .map(_._2)
          }
          .flattenOption

      // Calculate the wavelengths at where the peaks happen
      val calculatedCCDs =
        charts
          .flatMap(_.charts)
          .filter(_.chartType === ChartType.S2NChart)
          .flatMap { chart =>
            val maxFinalAt  = maxWavelength(chart, SeriesDataType.FinalS2NData)
            val maxSignalAt = maxWavelength(chart, SeriesDataType.SingleS2NData)
            val maxFinal    = maxValue(chart, SeriesDataType.FinalS2NData)
            val maxSignal   = maxValue(chart, SeriesDataType.SingleS2NData)

            ccds.zipWithIndex
              .map { (ccd, i) =>
                val finalWV        = maxFinalAt.lift(i)
                val singleWV       = maxSignalAt.lift(i)
                val maxFinalValue  = maxFinal.lift(i)
                val maxSingleValue = maxSignal.lift(i)

                (finalWV, singleWV, maxSingleValue, maxFinalValue).mapN {
                  (maxFinalAt, maxSingleAt, maxSingleValue, maxFinalValue) =>
                    ItcCcd(ccd.singleSNRatio,
                           maxSingleValue,
                           ccd.totalSNRatio,
                           maxFinalValue,
                           maxFinalAt,
                           maxSingleAt,
                           ccd.peakPixelFlux,
                           ccd.wellDepth,
                           ccd.ampGain,
                           ccd.warnings
                    )
                }
              }
              .toList
              .flattenOption
          }
      assert(calculatedCCDs.length === ccds.length)
      GraphResult(dataVersion, NonEmptyList.fromListUnsafe(calculatedCCDs), charts)
    }

  enum SNResultType(val tag: String) derives Enumerated:
    case Success          extends SNResultType("success")
    case SourceTooBright  extends SNResultType("source_too_bright")
    case BelowRange       extends SNResultType("below_range")
    case AboveRange       extends SNResultType("above_range")
    case NoData           extends SNResultType("no_data")
    case CalculationError extends SNResultType("calculation_error")

  sealed trait SNCalcResult extends Product with Serializable {
    def resultType: SNResultType
  }

  object SNCalcResult:
    given Encoder[SNCalcResult] = Encoder.instance { a =>
      Json
        .obj(("resultType", a.resultType.asJson))
        .deepMerge(a match {
          case s @ SNCalcSuccess(_)          => s.asJson
          case _: NoData                     => Json.Null
          case w @ WavelengthAtAboveRange(_) => w.asJson
          case w @ WavelengthAtBelowRange(_) => w.asJson
          case _                             => Json.Null
        })
    }

    case class SNCalcSuccess(
      signalToNoise: BigDecimal
    ) extends SNCalcResult
        derives Encoder.AsObject {
      val resultType = SNResultType.Success
    }

    case class NoData() extends SNCalcResult {
      val resultType = SNResultType.NoData
    }

    case class WavelengthAtBelowRange(signalToNoiseAt: Wavelength) extends SNCalcResult
        derives Encoder.AsObject {
      val resultType = SNResultType.BelowRange
    }

    case class WavelengthAtAboveRange(signalToNoiseAt: Wavelength) extends SNCalcResult
        derives Encoder.AsObject {
      val resultType = SNResultType.AboveRange
    }

    /** Generic calculation error */
    case class CalculationError(msg: String) extends SNCalcResult derives Encoder.AsObject {
      val resultType = SNResultType.CalculationError
    }
