// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.input

import cats.syntax.all.*
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.itc.SignificantFigures
import lucuma.odb.graphql.binding.*
import lucuma.odb.graphql.input.*

sealed trait SpectroscopyTimeInput {
  def atWavelength: Wavelength
  def signalToNoise: SignalToNoise
  def asterism: List[TargetDataInput]
  def constraints: ConstraintSetInput
  def mode: InstrumentModesInput
}

object SpectroscopyTimeInput:
  def unapply(
    arg: SpectroscopyTimeInput
  ): (Wavelength, SignalToNoise, List[TargetDataInput], ConstraintSetInput, InstrumentModesInput) =
    (arg.atWavelength, arg.signalToNoise, arg.asterism, arg.constraints, arg.mode)

case class SpectroscopyIntegrationTimeInput(
  atWavelength:  Wavelength,
  signalToNoise: SignalToNoise,
  asterism:      List[TargetDataInput],
  constraints:   ConstraintSetInput,
  mode:          InstrumentModesInput
) extends SpectroscopyTimeInput

object SpectroscopyIntegrationTimeInput {

  def binding: Matcher[SpectroscopyIntegrationTimeInput] =
    ObjectFieldsBinding.rmap {
      case List(
            WavelengthInput.Binding("atWavelength", wavelength),
            SignalToNoiseBinding("signalToNoise", signalToNoise),
            TargetDataInput.binding.List("asterism", asterism),
            ConstraintSetInput.Binding("constraints", constraints),
            InstrumentModesInput.binding("mode", mode)
          ) =>
        (wavelength, signalToNoise, asterism, constraints, mode).parMapN(apply)
    }

}

case class SpectroscopyIntegrationTimeAndGraphsInput(
  atWavelength:       Wavelength,
  signalToNoise:      SignalToNoise,
  asterism:           List[TargetDataInput],
  constraints:        ConstraintSetInput,
  mode:               InstrumentModesInput,
  significantFigures: Option[SignificantFigures]
) extends SpectroscopyTimeInput

object SpectroscopyIntegrationTimeAndGraphsInput {

  def binding: Matcher[SpectroscopyIntegrationTimeAndGraphsInput] =
    ObjectFieldsBinding.rmap {
      case List(
            WavelengthInput.Binding("atWavelength", wavelength),
            SignalToNoiseBinding("signalToNoise", signalToNoise),
            TargetDataInput.binding.List("asterism", asterism),
            ConstraintSetInput.Binding("constraints", constraints),
            InstrumentModesInput.binding("mode", mode),
            SignificantFiguresInput.binding.Option("significantFigures", significantFigures)
          ) =>
        (wavelength, signalToNoise, asterism, constraints, mode, significantFigures).parMapN(apply)
    }

}
