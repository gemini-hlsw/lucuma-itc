// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.legacy

import cats.data.NonEmptyList
import cats.syntax.all.*
import io.circe.Decoder
import lucuma.itc.ItcChartGroup

case class ItcRemoteResult(
  versionToken: String,
  ccds:         NonEmptyList[ItcRemoteCcd],
  groups:       NonEmptyList[ItcChartGroup]
) {
  def maxTotalSNRatio: Double = ccds.map(_.totalSNRatio).maximum
  def maxWellDepth: Double    = ccds.map(_.wellDepth).maximum
  def maxPeakPixelFlux: Int   = ccds.map(_.peakPixelFlux).maximum.toInt
}

case class ItcRemoteVersion(
  versionToken: String
) derives Decoder
