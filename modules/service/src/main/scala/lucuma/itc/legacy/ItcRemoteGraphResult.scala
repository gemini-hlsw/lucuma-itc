// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.legacy

import cats.data.NonEmptyList
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Decoder
import lucuma.core.math.SignalToNoise
import lucuma.itc.ItcChartGroup

case class GraphsRemoteResult(
  ccds:   NonEmptyList[ItcRemoteCcd],
  groups: NonEmptyList[ItcChartGroup]
) {
  val maxTotalSNRatio: Double = ccds.map(_.totalSNRatio).maximum
  val maxWellDepth: Double    = ccds.map(_.wellDepth).maximum
  val maxPeakPixelFlux: Int   = ccds.map(_.peakPixelFlux).maximum.toInt
}

case class ExposureCalculation(
  exposureTime:  Double,
  exposures:     PosInt,
  signalToNoise: SignalToNoise
)

case class ExposureTimeRemoteResult(exposureCalculation: ExposureCalculation)
