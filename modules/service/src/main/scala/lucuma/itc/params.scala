// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import cats.Hash
import cats.derived.*
import io.circe.*
import lucuma.core.enums.*
import lucuma.itc.search.ObservingMode.SpectroscopyMode.*
import lucuma.itc.search.*

sealed trait SpectroscopyParams

case class GmosNSpectroscopyParams(
  grating: GmosNorthGrating,
  fpu:     GmosNorthFpuParam,
  filter:  Option[GmosNorthFilter]
) extends SpectroscopyParams
    derives Encoder.AsObject

case class GmosSSpectroscopyParams(
  grating: GmosSouthGrating,
  fpu:     GmosSouthFpuParam,
  filter:  Option[GmosSouthFilter]
) extends SpectroscopyParams
    derives Encoder.AsObject

sealed trait ImagingParams

case class GmosNImagingParams(filter: GmosNorthFilter) extends ImagingParams
    derives Encoder.AsObject

case class GmosSImagingParams(filter: GmosSouthFilter) extends ImagingParams
    derives Encoder.AsObject

case class ItcObservingConditions(
  iq:      ImageQuality,
  cc:      CloudExtinction,
  wv:      WaterVapor,
  sb:      SkyBackground,
  airmass: Double
) derives Hash
