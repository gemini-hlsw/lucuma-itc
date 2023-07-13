// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.input

import cats.syntax.parallel.*
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.enums.Band
import lucuma.core.model.SourceProfile
import lucuma.core.math.RadialVelocity
import lucuma.odb.graphql.binding.*
import lucuma.odb.graphql.input.*
import lucuma.odb.graphql.input.sourceprofile.*

case class SpectroscopyIntegrationTimeInput(
  wavelength:      Wavelength,
  signalToNoiseAt: Option[Wavelength],
  signalToNoise:   SignalToNoise,
  sourceProfile:   SourceProfile,
  band:            Band,
  radialVelocity:  RadialVelocity,
  constraints:     ConstraintSetInput,
  mode:            InstrumentModesInput
)

object SpectroscopyIntegrationTimeInput {

  def binding: Matcher[SpectroscopyIntegrationTimeInput] =
    ObjectFieldsBinding.rmap {
      case List(
            WavelengthInput.Binding("wavelength", wavelength),
            WavelengthInput.Binding.Option("signalToNoiseAt", signalToNoiseAt),
            SignalToNoiseBinding("signalToNoise", signalToNoise),
            SourceProfileInput.CreateBinding("sourceProfile", sourceProfile),
            BandBinding("band", band),
            RadialVelocityInput.Binding("radialVelocity", radialVelocity),
            ConstraintSetInput.Binding("constraints", constraints),
            InstrumentModesInput.binding("mode", mode)
          ) =>
        (wavelength,
         signalToNoiseAt,
         signalToNoise,
         sourceProfile,
         band,
         radialVelocity,
         constraints,
         mode
        ).parMapN(apply)
    }

}
