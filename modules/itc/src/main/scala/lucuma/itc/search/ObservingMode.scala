// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.search

import lucuma.core.enum._
import lucuma.core.math.Wavelength
import lucuma.itc.ItcObservationDetails
import lucuma.itc.search.syntax.gmosnorthdisperser._
import lucuma.itc.search.syntax.gmosnorthfilter._
import lucuma.itc.search.syntax.gmosnorthfpu._
import lucuma.itc.search.syntax.gmossouthdisperser._
import lucuma.itc.search.syntax.gmossouthfilter._
import lucuma.itc.search.syntax.gmossouthfpu._
import spire.math.Rational

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

    final case class GmosNorth(
      λ:         Wavelength,
      disperser: GmosNorthDisperser,
      fpu:       GmosNorthFpu,
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

    final case class GmosSouth(
      λ:         Wavelength,
      disperser: GmosSouthDisperser,
      fpu:       GmosSouthFpu,
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
  }

}
