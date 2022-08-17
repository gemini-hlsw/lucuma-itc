// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.search

import io.circe.*
import io.circe.syntax.*
import lucuma.core.enums._
import lucuma.core.math.Angle
import lucuma.core.math.Coverage
import lucuma.core.math.Wavelength
import lucuma.itc.GmosNITCParams
import lucuma.itc.GmosSITCParams
import lucuma.itc.encoders.given
import lucuma.itc.search.syntax.gmosnorthfilter._
import lucuma.itc.search.syntax.gmosnorthfpu._
import lucuma.itc.search.syntax.gmossouthfilter._
import lucuma.itc.search.syntax.gmossouthfpu._
import spire.math.Rational

case class GmosNorthFpuParam(
  builtin: GmosNorthFpu
) derives Encoder.AsObject {
  def isIfu: Boolean            = builtin.isIfu
  def effectiveSlitWidth: Angle = builtin.effectiveSlitWidth
}

case class GmosSouthFpuParam(
  builtin: GmosSouthFpu
) derives Encoder.AsObject {
  def isIfu: Boolean            = builtin.isIfu
  def effectiveSlitWidth: Angle = builtin.effectiveSlitWidth
}

sealed trait ObservingMode {
  def instrument: Instrument
  def analysisMethod: ItcObservationDetails.AnalysisMethod
}

object ObservingMode {

  sealed trait Spectroscopy extends ObservingMode {
    def λ: Wavelength
    def resolution: Rational
    def coverage: Coverage
  }

  object Spectroscopy {
    given Encoder[ObservingMode.Spectroscopy] = Encoder.instance {
      case gn: GmosNorth => gn.asJson
      case gs: GmosSouth => gs.asJson
    }

    sealed trait GmosSpectroscopy extends Spectroscopy {
      def isIfu: Boolean

      def analysisMethod: ItcObservationDetails.AnalysisMethod =
        if (isIfu)
          ItcObservationDetails.AnalysisMethod.Ifu.Single(
            skyFibres = 250,
            offset = 5.0
          )
        else
          ItcObservationDetails.AnalysisMethod.Aperture.Auto(
            skyAperture = 5.0
          )
    }

    case class GmosNorth(
      λ:         Wavelength,
      disperser: GmosNorthGrating,
      fpu:       GmosNorthFpuParam,
      filter:    Option[GmosNorthFilter]
    ) extends GmosSpectroscopy {
      val isIfu = fpu.isIfu

      val instrument: Instrument =
        Instrument.GmosNorth

      def resolution: Rational =
        disperser.resolution(λ, fpu.effectiveSlitWidth)

      def coverage: Coverage =
        filter.foldLeft(disperser.coverage(λ))(_ ⋂ _.coverage)
    }

    object GmosNorth:
      given Encoder[GmosNorth] = a =>
        Json.obj(
          ("instrument", Json.fromString(a.instrument.longName.toUpperCase.replace(" ", "_"))),
          ("resolution", Json.fromInt(a.resolution.toInt)),
          ("params", GmosNITCParams(a.disperser, a.fpu, a.filter).asJson),
          ("wavelength", a.λ.asJson)
        )

    case class GmosSouth(
      λ:         Wavelength,
      disperser: GmosSouthGrating,
      fpu:       GmosSouthFpuParam,
      filter:    Option[GmosSouthFilter]
    ) extends GmosSpectroscopy {
      val isIfu = fpu.isIfu

      val instrument: Instrument =
        Instrument.GmosSouth

      def resolution: Rational =
        disperser.resolution(λ, fpu.effectiveSlitWidth)

      def coverage: Coverage =
        filter.foldLeft(disperser.coverage(λ))(_ ⋂ _.coverage)
    }

    object GmosSouth:
      given Encoder[GmosSouth] = a =>
        Json.obj(
          ("instrument", Json.fromString(a.instrument.longName.toUpperCase.replace(" ", "_"))),
          ("resolution", Json.fromInt(a.resolution.toInt)),
          ("params", GmosSITCParams(a.disperser, a.fpu, a.filter).asJson),
          ("wavelength", a.λ.asJson)
        )

  }

}
