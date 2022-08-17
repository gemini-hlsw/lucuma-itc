// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.*
import io.circe.generic.semiauto._
import io.circe.syntax.*
import lucuma.core.enums._
import lucuma.core.math.Wavelength
import lucuma.itc.search.ObservingMode.Spectroscopy._
import lucuma.itc.search.*

import java.math.RoundingMode
import scala.concurrent.duration.FiniteDuration

sealed trait SpectroscopyParams

final case class GmosNITCParams(
  grating: GmosNorthGrating,
  fpu:     GmosNorthFpuParam,
  filter:  Option[GmosNorthFilter]
) extends SpectroscopyParams
    derives Encoder.AsObject

final case class GmosSITCParams(
  grating: GmosSouthGrating,
  fpu:     GmosSouthFpuParam,
  filter:  Option[GmosSouthFilter]
) extends SpectroscopyParams
    derives Encoder.AsObject

case class ItcObservingConditions(
  iq:      ImageQuality,
  cc:      CloudExtinction,
  wv:      WaterVapor,
  sb:      SkyBackground,
  airmass: Double
)
