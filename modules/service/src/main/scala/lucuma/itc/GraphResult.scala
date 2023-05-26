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
import lucuma.itc.service.*

case class GraphResult(
  ccds:                NonEmptyList[ItcCcd],
  charts:              NonEmptyList[ItcChartGroup],
  peakSNRatio:         SignalToNoise,
  atWavelengthSNRatio: Option[SignalToNoise]
)

object GraphResult:
  def fromLegacy(
    ccds:            NonEmptyList[ItcRemoteCcd],
    charts:          NonEmptyList[ItcChartGroup],
    signalToNoiseAt: Option[Wavelength]
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

    def valueAt(chart: ItcChart, seriesDataType: SeriesDataType): Option[SignalToNoise] =
      chart.series
        .filter(_.seriesType === seriesDataType)
        .zip(ccds.toList)
        .map { case (sn, _) =>
          signalToNoiseAt
            .flatMap(at => sn.data.find(_._1 >= at.toNanometers.value.value))
            .map(_._2)
        }
        .flattenOption
        .headOption
        .flatMap(v => SignalToNoise.FromBigDecimalRounding.getOption(v))

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

    val maxTotalSNRatio = calculatedCCDs.map(_.maxTotalSNRatio).max
    val peakSNRatio     = SignalToNoise.FromBigDecimalRounding
      .getOption(maxTotalSNRatio)
      .getOrElse(throw UpstreamException("Peak SN ratio is not a number"))
    val wvAtRatio       =
      charts
        .flatMap(_.charts)
        .filter(_.chartType === ChartType.S2NChart)
        .map(c => valueAt(c, SeriesDataType.FinalS2NData))
        .collect { case Some(v) => v }
        .headOption

    assert(calculatedCCDs.length === ccds.length)
    GraphResult(NonEmptyList.fromListUnsafe(calculatedCCDs), charts, peakSNRatio, wvAtRatio)
  }
