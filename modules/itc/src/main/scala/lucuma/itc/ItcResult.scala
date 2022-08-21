// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import cats.Semigroup
import cats.data.NonEmptyList
import cats.implicits._
import io.circe.Decoder
import io.circe.HCursor
import lucuma.itc.syntax.all.given

case class ItcResult(versionToken: String, ccds: NonEmptyList[ItcCcd]) {

  // We may not need these
  def maxPeakPixelFlux: Int      = ccds.map(_.peakPixelFlux).maximum.toInt
  def maxAdu: Int                = ccds.map(_.adu).maximum
  def maxPercentFullWell: Double = ccds.map(_.percentFullWell).maximum
  def maxWellDepth: Double       = ccds.map(_.wellDepth).maximum
  def maxSingleSNRatio: Double   = ccds.map(_.singleSNRatio).maximum
  def maxTotalSNRatio: Double    = ccds.map(_.totalSNRatio).maximum

}
