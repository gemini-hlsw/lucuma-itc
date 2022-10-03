// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import io.circe.Decoder
import io.circe.Encoder
import lucuma.core.math.Wavelength
import lucuma.itc.ItcWarning
import lucuma.itc.encoders.given

case class ItcCcd(
  singleSNRatio:                 Double,          // the final SN ratio for a single image
  maxSingleSNRatio:              Double,          // the max single SN ratio for a single image/ccd
  totalSNRatio:                  Double,          // the total SN ratio for all images
  maxTotalSNRatio:               Double,          // the max final SN ratio for all images/ccd
  wavelengthForMaxTotalSNRatio:  Wavelength,      // Wavelength where we get the max total SN
  wavelengthForMaxSingleSNRatio: Wavelength,      // Wavelength where we get the max single SN
  peakPixelFlux:                 Double,          // the highest e- count for all pixels on the CCD
  wellDepth:                     Double,          // the well depth (max e- count per pixel) for this CCD
  ampGain:                       Double,          // the amplifier gain for this CCD (used to calculate ADU)
  warnings:                      List[ItcWarning] // the warnings provided by ITC for this CCD
) derives Encoder.AsObject {

  // the max percentage of the well saturation for peak pixel
  val percentFullWell: Double =
    peakPixelFlux / wellDepth * 100.0

  // the ADU value
  val adu: Int =
    (peakPixelFlux / ampGain).toInt

}
