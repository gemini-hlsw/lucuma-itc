// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import cats.data.Chain
import cats.data.NonEmptyChain
import cats.syntax.all.*
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.itc.legacy.ItcRemoteCcd

case class TargetGraphsCalcResult(
  ccds:                      NonEmptyChain[ItcCcd],
  data:                      NonEmptyChain[ItcGraphGroup],
  peakFinalSNRatio:          FinalSN,
  atWavelengthFinalSNRatio:  Option[FinalSN],
  peakSingleSNRatio:         SingleSN,
  atWavelengthSingleSNRatio: Option[SingleSN]
)

object TargetGraphsCalcResult:
  def fromLegacy(
    ccds:            NonEmptyChain[ItcRemoteCcd],
    originalGraphs:  NonEmptyChain[ItcGraphGroup],
    signalToNoiseAt: Option[Wavelength]
  ): TargetGraphsCalcResult = {
    val graphs: NonEmptyChain[ItcGraphGroup] =
      originalGraphs.map: graph =>
        graph.copy(graphs = graph.graphs.map: c =>
          c.copy(series = c.series.map: s =>
            ItcSeries(
              s.title,
              s.seriesType,
              s.data.collect { case (x, y) if !y.isNaN => (x.toDouble, y.toDouble) }
            )))

    def maxWavelength(graph: ItcGraph, seriesDataType: SeriesDataType): List[Wavelength] =
      graph.series
        .filter(_.seriesType === seriesDataType)
        .zip(ccds.toList)
        .map: (sn, _) =>
          sn.data
            .maxByOption(_._2)
            .map(_._1)
            .flatMap(d => Wavelength.intPicometers.getOption((d * 1000).toInt))
        .flattenOption

    def valueAt(graph: ItcGraph, seriesDataType: SeriesDataType): Option[SignalToNoise] =
      graph.series
        .filter(_.seriesType === seriesDataType)
        .zip(ccds.toList)
        .map: (sn, _) =>
          signalToNoiseAt
            .flatMap(at => sn.data.find(_._1 >= at.toNanometers.value.value))
            .map(_._2)
        .flattenOption
        .headOption
        .flatMap(v => SignalToNoise.FromBigDecimalRounding.getOption(v))

    def maxValue(graph: ItcGraph, seriesDataType: SeriesDataType): List[Double] =
      graph.series
        .filter(_.seriesType === seriesDataType)
        .zip(ccds.toList)
        .map: (sn, _) =>
          sn.data
            .maxByOption(_._2)
            .map(_._2)
        .flattenOption

    // Calculate the wavelengths at where the peaks happen
    val calculatedCCDs: Chain[ItcCcd] =
      graphs
        .flatMap(_.graphs)
        .filter(_.graphType === GraphType.S2NGraph)
        .flatMap: graph =>
          val maxFinalAt  = maxWavelength(graph, SeriesDataType.FinalS2NData)
          val maxSignalAt = maxWavelength(graph, SeriesDataType.SingleS2NData)
          val maxFinal    = maxValue(graph, SeriesDataType.FinalS2NData)
          val maxSignal   = maxValue(graph, SeriesDataType.SingleS2NData)

          ccds.zipWithIndex
            .map: (ccd, i) =>
              val finalWV        = maxFinalAt.lift(i)
              val singleWV       = maxSignalAt.lift(i)
              val maxFinalValue  = maxFinal.lift(i)
              val maxSingleValue = maxSignal.lift(i)

              (finalWV, singleWV, maxSingleValue, maxFinalValue).mapN:
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
            .toChain
            .flattenOption

    val maxTotalSNRatio: Double          =
      calculatedCCDs
        .map(_.maxTotalSNRatio)
        .maximumOption
        .getOrElse(throw UpstreamException(List("CCD List is empty")))
    val peakFinalSNRatio: SignalToNoise  = SignalToNoise.FromBigDecimalRounding
      .getOption(maxTotalSNRatio)
      .getOrElse(throw UpstreamException(List("Peak Total SN is not a number")))
    val maxSingleSNRatio: Double         = calculatedCCDs
      .map(_.maxSingleSNRatio)
      .maximumOption
      .getOrElse(throw UpstreamException(List("CCD List is empty")))
    val peakSingleSNRatio: SignalToNoise = SignalToNoise.FromBigDecimalRounding
      .getOption(maxSingleSNRatio)
      .getOrElse(throw UpstreamException(List("Peak Single SN is not a number")))

    def wvAtRatio(seriesType: SeriesDataType) =
      graphs
        .flatMap(_.graphs)
        .filter(_.graphType === GraphType.S2NGraph)
        .map(c => valueAt(c, seriesType))
        .collect { case Some(v) => v }
        .headOption

    val wvAtFinalRatio  = wvAtRatio(SeriesDataType.FinalS2NData)
    val wvAtSingleRatio = wvAtRatio(SeriesDataType.SingleS2NData)

    assert(calculatedCCDs.length === ccds.length.toInt)
    TargetGraphsCalcResult(
      NonEmptyChain.fromChainUnsafe(calculatedCCDs),
      graphs,
      FinalSN(peakFinalSNRatio),
      wvAtFinalRatio.map(FinalSN.apply(_)),
      SingleSN(peakSingleSNRatio),
      wvAtSingleRatio.map(SingleSN.apply(_))
    )
  }
