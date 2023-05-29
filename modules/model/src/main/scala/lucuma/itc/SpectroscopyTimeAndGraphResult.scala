// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import cats.data.NonEmptyList
import cats.syntax.all.*
import eu.timepit.refined.*
import eu.timepit.refined.numeric.NonNegative
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.PosInt
import io.circe.CursorOp
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Encoder
import io.circe.HCursor
import io.circe.Json
import io.circe.generic.semiauto.*
import io.circe.refined.*
import lucuma.core.enums.*
import lucuma.core.math.SignalToNoise
import lucuma.core.syntax.string.*
import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan
import lucuma.itc.encoders.given
import lucuma.itc.math.*

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
  ): SpectroscopyTimeAndGraphResult = {
    println(graph)
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
  }
