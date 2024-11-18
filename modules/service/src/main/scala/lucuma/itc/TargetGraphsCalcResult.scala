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

    // Find the wavelength that gives the maximum value for the given series data type
    // It returns one value per ccd, returns a pair of wavelength and value
    def wavelengthAtMaxSN(
      graph:          ItcGraph,
      seriesDataType: SeriesDataType
    ): List[(Wavelength, Double)] =
      graph.series
        .filter(_.seriesType === seriesDataType)
        .zip(ccds.toList)
        .map: (sn, _) =>
          sn.data
            .filter(_._1 > 0)
            .maxByOption(_._2)
            .flatMap((w, s) => Wavelength.intPicometers.getOption((w * 1000).toInt).tupleRight(s))
        .flattenOption

    def signalToNoiseAtWv(graph: ItcGraph, seriesDataType: SeriesDataType): Option[SignalToNoise] =
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

    // Calculate the wavelengths at where the peaks happen
    val calculatedCCDs: Chain[ItcCcd] =
      graphs
        .flatMap(_.graphs)
        .filter(_.graphType === GraphType.S2NGraph)
        .flatMap: graph =>
          val finalSN     = wavelengthAtMaxSN(graph, SeriesDataType.FinalS2NData)
          val maxWVFinal  = finalSN.map(_._1)
          val maxSNFinal  = finalSN.map(_._2)
          val singleSN    = wavelengthAtMaxSN(graph, SeriesDataType.SingleS2NData)
          val maxWVSingle = singleSN.map(_._1)
          val maxSNSingle = singleSN.map(_._2)

          ccds.zipWithIndex
            .map: (ccd, i) =>
              val finalWV        = maxWVFinal.lift(i)
              val singleWV       = maxWVSingle.lift(i)
              val maxFinalValue  = maxSNFinal.lift(i)
              val maxSingleValue = maxSNSingle.lift(i)

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
        .map(c => signalToNoiseAtWv(c, seriesType))
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
