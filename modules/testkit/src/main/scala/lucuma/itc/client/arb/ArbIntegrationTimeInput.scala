// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client
package arb

import cats.data.NonEmptyList
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.math.arb.ArbSignalToNoise
import lucuma.core.math.arb.ArbWavelength
import lucuma.core.model.ConstraintSet
import lucuma.core.model.arb.ArbConstraintSet
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary

trait ArbIntegrationTimeInput {
  import ArbConstraintSet.given
  import ArbInstrumentMode.given
  import ArbSignalToNoise.given
  import ArbTargetInput.given
  import ArbWavelength.given

  given [A: Arbitrary]: Arbitrary[NonEmptyList[A]] =
    Arbitrary:
      arbitrary[List[A]].suchThat(_.nonEmpty).map(NonEmptyList.fromListUnsafe)

  given Arbitrary[SpectroscopyIntegrationTimeParameters] =
    Arbitrary:
      for
        w   <- arbitrary[Wavelength]
        s2n <- arbitrary[SignalToNoise]
        cs  <- arbitrary[ConstraintSet]
        im  <- arbitrary[InstrumentMode]
      yield SpectroscopyIntegrationTimeParameters(w, s2n, cs, im)

  given Arbitrary[SpectroscopyIntegrationTimeInput] =
    Arbitrary:
      for
        pars <- arbitrary[SpectroscopyIntegrationTimeParameters]
        ast  <- arbitrary[NonEmptyList[TargetInput]]
      yield SpectroscopyIntegrationTimeInput(pars, ast)

  given Cogen[SpectroscopyIntegrationTimeInput] =
    Cogen[
      (Wavelength, SignalToNoise, List[TargetInput], ConstraintSet, InstrumentMode)
    ].contramap: a =>
      (a.atWavelength, a.signalToNoise, a.asterism.toList, a.constraints, a.mode)

  given Arbitrary[ImagingIntegrationTimeParameters] =
    Arbitrary:
      for {
        w   <- arbitrary[Wavelength]
        s2n <- arbitrary[SignalToNoise]
        cs  <- arbitrary[ConstraintSet]
        im  <- arbitrary[InstrumentMode]
      } yield ImagingIntegrationTimeParameters(w, s2n, cs, im)

  given Arbitrary[ImagingIntegrationTimeInput] =
    Arbitrary:
      for
        pars <- arbitrary[ImagingIntegrationTimeParameters]
        ast  <- arbitrary[NonEmptyList[TargetInput]]
      yield ImagingIntegrationTimeInput(pars, ast)

  given Cogen[ImagingIntegrationTimeInput] =
    Cogen[
      (
        Wavelength,
        SignalToNoise,
        List[TargetInput],
        ConstraintSet,
        InstrumentMode
      )
    ].contramap: a =>
      (a.atWavelength, a.signalToNoise, a.asterism.toList, a.constraints, a.mode)
}

object ArbIntegrationTimeInput extends ArbIntegrationTimeInput
