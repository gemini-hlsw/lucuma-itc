// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.legacy

import cats.data.NonEmptyList
import cats.syntax.all.*
import io.circe.Decoder
import lucuma.itc.ItcChartGroup

case class GraphsRemoteResult(
  ccds:   NonEmptyList[ItcRemoteCcd],
  groups: NonEmptyList[ItcChartGroup]
) {
  val maxTotalSNRatio: Double = ccds.map(_.totalSNRatio).maximum
  val maxWellDepth: Double    = ccds.map(_.wellDepth).maximum
  val maxPeakPixelFlux: Int   = ccds.map(_.peakPixelFlux).maximum.toInt
}

case class ExposureCalculation(exposureTime: Double, exposures: Int, signalToNoise: Double)

case class ExposureTimeRemoteResult(exposureCalculation: ExposureCalculation)
