// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.legacy

import cats.data.NonEmptyChain
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.itc.ItcGraphGroup
import lucuma.itc.SingleSN
import lucuma.itc.FinalSN

case class GraphsRemoteResult(
  ccds:   NonEmptyChain[ItcRemoteCcd],
  groups: NonEmptyChain[ItcGraphGroup]
) {
  val maxTotalSNRatio: Double = ccds.map(_.totalSNRatio).maximum
  val maxWellDepth: Double    = ccds.map(_.wellDepth).maximum
  val maxPeakPixelFlux: Int   = ccds.map(_.peakPixelFlux).maximum.toInt
}

case class ExposureCalculation(
  exposureTime:  Double,
  exposureCount: NonNegInt
)

case class SignalToNoiseAt(
  wavelength: Wavelength,
  single:     SingleSN,
  total:      FinalSN
)

case class AllExposureCalculations(
  exposureCalculations: NonEmptyChain[ExposureCalculation],
  selectedIndex:        Int
)

case class IntegrationTimeRemoteResult(
  // If we request time/counts we would gett a None, otherwise we get at least one result.
  // Gmos spec returns one, thte selecte index
  // Gmos img returns one per ccd, we use the first
  exposureCalculation: Option[AllExposureCalculations],

  // In principle this should not be empty, but you could still use a wv outside the rang of a ccd. Rather than giving a zero SN, we just don't return anything.
  signalToNoiseAt: Option[SignalToNoiseAt]
)
