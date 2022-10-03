// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.legacy

import cats.data.NonEmptyList
import cats.syntax.all._

case class ItcRemoteResult(versionToken: String, ccds: NonEmptyList[ItcRemoteCcd]) {

  // We may not need these
  def maxPeakPixelFlux: Int    = ccds.map(_.peakPixelFlux).maximum.toInt
  def maxWellDepth: Double     = ccds.map(_.wellDepth).maximum
  def maxSingleSNRatio: Double = ccds.map(_.singleSNRatio).maximum
  def maxTotalSNRatio: Double  = ccds.map(_.totalSNRatio).maximum

}
