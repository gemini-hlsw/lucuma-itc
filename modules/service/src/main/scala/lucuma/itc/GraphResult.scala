// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import cats.data.NonEmptyList
import cats.syntax.all.*
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.itc.legacy.ItcRemoteCcd

case class GraphResult(
  ccds:                      NonEmptyList[ItcCcd],
  charts:                    NonEmptyList[ItcChartGroup],
  peakFinalSNRatio:          FinalSN,
  atWavelengthFinalSNRatio:  Option[FinalSN],
  peakSingleSNRatio:         SingleSN,
  atWavelengthSingleSNRatio: Option[SingleSN]
)

object GraphResult:
  def fromLegacy(
    ccds:            NonEmptyList[ItcRemoteCcd],
    originalCharts:  NonEmptyList[ItcChartGroup],
    signalToNoiseAt: Option[Wavelength]
  ): GraphResult = {
    val charts = originalCharts.map { chart =>
      chart.copy(charts = chart.charts.map { c =>
        c.copy(series = c.series.map { s =>
          ItcSeries(s.title,
                    s.seriesType,
                    s.data.collect { case (x, y) if !y.isNaN => (x.toDouble, y.toDouble) }
          )
        })
      })
    }

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

    val maxTotalSNRatio   = calculatedCCDs.map(_.maxTotalSNRatio).max
    val peakFinalSNRatio  = SignalToNoise.FromBigDecimalRounding
      .getOption(maxTotalSNRatio)
      .getOrElse(throw UpstreamException(List("Peak Total SN is not a number")))
    val maxSingleSNRatio  = calculatedCCDs.map(_.maxSingleSNRatio).max
    val peakSingleSNRatio = SignalToNoise.FromBigDecimalRounding
      .getOption(maxSingleSNRatio)
      .getOrElse(throw UpstreamException(List("Peak Single SN is not a number")))

    def wvAtRatio(seriesType: SeriesDataType) =
      charts
        .flatMap(_.charts)
        .filter(_.chartType === ChartType.S2NChart)
        .map(c => valueAt(c, seriesType))
        .collect { case Some(v) => v }
        .headOption

    val wvAtFinalRatio  = wvAtRatio(SeriesDataType.FinalS2NData)
    val wvAtSingleRatio = wvAtRatio(SeriesDataType.SingleS2NData)

    assert(calculatedCCDs.length === ccds.length)
    GraphResult(
      NonEmptyList.fromListUnsafe(calculatedCCDs),
      charts,
      FinalSN(peakFinalSNRatio),
      wvAtFinalRatio.map(FinalSN.apply(_)),
      SingleSN(peakSingleSNRatio),
      wvAtSingleRatio.map(SingleSN.apply(_))
    )
  }
