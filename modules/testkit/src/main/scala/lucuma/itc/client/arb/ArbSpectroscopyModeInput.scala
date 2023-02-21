// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client
package arb

//import eu.timepit.refined.scalacheck.all.*
import eu.timepit.refined.types.numeric.PosBigDecimal
import lucuma.core.enums.Band
import lucuma.core.math.RadialVelocity
import lucuma.core.math.Wavelength
import lucuma.core.math.arb.ArbRadialVelocity
import lucuma.core.math.arb.ArbRefined
import lucuma.core.math.arb.ArbWavelength
import lucuma.core.model.ConstraintSet
import lucuma.core.model.SourceProfile
import lucuma.core.model.arb.ArbConstraintSet
import lucuma.core.model.arb.ArbSourceProfile
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.*

trait ArbSpectroscopyModeInput {

  import ArbConstraintSet.*
  import ArbEnumerated.*
  import ArbInstrumentMode.given
  import ArbRadialVelocity.*
  import ArbRefined.given
  import ArbSourceProfile.given
  import ArbWavelength.*

  def genSpectroscopyModeInput(
    im: InstrumentMode
  ): Gen[SpectroscopyModeInput] =
    for {
      w   <- arbitrary[Wavelength]
      s2n <- arbitrary[PosBigDecimal]
      sat <- arbitrary[Option[Wavelength]]
      sp  <- arbitrary[SourceProfile]
      b   <- arbitrary[Band]
      rv  <- arbitrary[RadialVelocity]
      cs  <- arbitrary[ConstraintSet]
      im  <- arbitrary[InstrumentMode]
    } yield SpectroscopyModeInput(
      w,
      s2n,
      sat,
      sp,
      b,
      rv,
      cs,
      im
    )

  given Arbitrary[SpectroscopyModeInput] =
    Arbitrary {
      for {
        im <- arbitrary[InstrumentMode]
        sm <- genSpectroscopyModeInput(im)
      } yield sm
    }

  given Cogen[SpectroscopyModeInput] =
    Cogen[
      (
        Wavelength,
        PosBigDecimal,
        Option[Wavelength],
        SourceProfile,
        Band,
        RadialVelocity,
        ConstraintSet,
        InstrumentMode
      )
    ].contramap { a =>
      (
        a.wavelength,
        a.signalToNoise,
        a.signalToNoiseAt,
        a.sourceProfile,
        a.band,
        a.radialVelocity,
        a.constraints,
        a.mode
      )
    }
}

object ArbSpectroscopyModeInput extends ArbSpectroscopyModeInput