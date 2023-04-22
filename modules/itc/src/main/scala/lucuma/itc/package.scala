// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import cats.data.NonEmptyList
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosLong
import io.circe.*
import io.circe.syntax.*
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.model.NonNegDuration
import lucuma.core.util.Enumerated
import lucuma.itc.encoders.given
import lucuma.itc.legacy.ItcRemoteCcd
import lucuma.itc.search.*

import scala.concurrent.duration.FiniteDuration

case class UpstreamException(msg: String) extends RuntimeException(msg)

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
        .map { case (sn, _) =>
          sn.data
            .maxByOption(_._2)
            .map(_._1)
            .flatMap(d => Wavelength.intPicometers.getOption((d * 1000).toInt))
        }
        .flattenOption

    def maxValue(chart: ItcChart, seriesDataType: SeriesDataType): List[Double] =
      chart.series
        .filter(_.seriesType === seriesDataType)
        .zip(ccds.toList)
        .map { case (sn, _) =>
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
    signalToNoise: SignalToNoise
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