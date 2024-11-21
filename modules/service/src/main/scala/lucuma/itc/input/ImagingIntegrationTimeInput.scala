// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.input

import cats.syntax.parallel.*
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.odb.graphql.binding.*
import lucuma.odb.graphql.input.*

case class ImagingIntegrationTimeInput(
  atWavelength:  Wavelength,
  signalToNoise: SignalToNoise,
  asterism:      List[TargetDataInput],
  constraints:   ConstraintSetInput,
  mode:          InstrumentModesInput
)

object ImagingIntegrationTimeInput {

  def binding: Matcher[ImagingIntegrationTimeInput] =
    ObjectFieldsBinding.rmap {
      case List(
            WavelengthInput.Binding("wavelength", wavelength),
            SignalToNoiseBinding("signalToNoise", signalToNoise),
            TargetDataInput.binding.List("asterism", asterism),
            ConstraintSetInput.Binding("constraints", constraints),
            InstrumentModesInput.binding("mode", mode)
          ) =>
        (wavelength, signalToNoise, asterism, constraints, mode).parMapN(
          apply
        )
    }

}
