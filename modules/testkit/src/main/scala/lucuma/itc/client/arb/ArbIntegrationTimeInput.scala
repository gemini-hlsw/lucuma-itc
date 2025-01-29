// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client
package arb

import cats.data.NonEmptyList
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.arb.ArbConstraintSet
import lucuma.core.model.arb.ArbExposureTimeMode
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary

trait ArbIntegrationTimeInput {
  import ArbConstraintSet.given
  import ArbExposureTimeMode.given
  import ArbInstrumentMode.given
  import ArbTargetInput.given

  given [A: Arbitrary]: Arbitrary[NonEmptyList[A]] =
    Arbitrary:
      arbitrary[List[A]].suchThat(_.nonEmpty).map(NonEmptyList.fromListUnsafe)

  given Arbitrary[SpectroscopyIntegrationTimeParameters] =
    Arbitrary:
      for
        ex <- arbitrary[ExposureTimeMode]
        cs <- arbitrary[ConstraintSet]
        im <- arbitrary[InstrumentMode]
      yield SpectroscopyIntegrationTimeParameters(ex, cs, im)

  given Arbitrary[SpectroscopyIntegrationTimeInput] =
    Arbitrary:
      for
        pars <- arbitrary[SpectroscopyIntegrationTimeParameters]
        ast  <- arbitrary[NonEmptyList[TargetInput]]
      yield SpectroscopyIntegrationTimeInput(pars, ast)

  given Cogen[SpectroscopyIntegrationTimeInput] =
    Cogen[
      (ExposureTimeMode, List[TargetInput], ConstraintSet, InstrumentMode)
    ].contramap: a =>
      (a.exposureTimeMode, a.asterism.toList, a.constraints, a.mode)

  given Arbitrary[ImagingIntegrationTimeParameters] =
    Arbitrary:
      for {
        ex <- arbitrary[ExposureTimeMode]
        cs <- arbitrary[ConstraintSet]
        im <- arbitrary[InstrumentMode]
      } yield ImagingIntegrationTimeParameters(ex, cs, im)

  given Arbitrary[ImagingIntegrationTimeInput] =
    Arbitrary:
      for
        pars <- arbitrary[ImagingIntegrationTimeParameters]
        ast  <- arbitrary[NonEmptyList[TargetInput]]
      yield ImagingIntegrationTimeInput(pars, ast)

  given Cogen[ImagingIntegrationTimeInput] =
    Cogen[
      (
        ExposureTimeMode,
        List[TargetInput],
        ConstraintSet,
        InstrumentMode
      )
    ].contramap: a =>
      (a.exposureTimeMode, a.asterism.toList, a.constraints, a.mode)
}

object ArbIntegrationTimeInput extends ArbIntegrationTimeInput
