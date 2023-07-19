// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import cats.data.NonEmptyList
import eu.timepit.refined.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Encoder
import io.circe.generic.semiauto.*
import lucuma.core.math.SignalToNoise
import lucuma.core.util.TimeSpan
import lucuma.itc.encoders.given

case class SpectroscopyTimeAndGraphResult(
  serverVersion:             String,
  dataVersion:               String,
  exposureTime:              TimeSpan,
  exposures:                 PosInt,
  ccds:                      NonEmptyList[ItcCcd],
  charts:                    NonEmptyList[ItcChart],
  peakFinalSNRatio:          FinalSN,
  atWavelengthFinalSNRatio:  Option[FinalSN],
  peakSingleSNRatio:         SingleSN,
  atWavelengthSingleSNRatio: Option[SingleSN]
) derives Encoder.AsObject

object SpectroscopyTimeAndGraphResult:
  def fromTimeAndGraph(
    exposureTime: TimeSpan,
    exposures:    PosInt,
    graph:        SpectroscopyGraphResult
  ): SpectroscopyTimeAndGraphResult =
    SpectroscopyTimeAndGraphResult(
      graph.serverVersion,
      graph.dataVersion,
      exposureTime,
      exposures,
      graph.ccds,
      graph.charts,
      graph.peakFinalSNRatio,
      graph.atWavelengthFinalSNRatio,
      graph.peakSingleSNRatio,
      graph.atWavelengthSingleSNRatio
    )
