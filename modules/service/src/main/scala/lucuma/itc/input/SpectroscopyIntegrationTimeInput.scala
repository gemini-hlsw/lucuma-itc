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
  def wavelength: Wavelength
  def signalToNoise: SignalToNoise
  def signalToNoiseAt: Option[Wavelength]
  def asterism: List[TargetDataInput]
  def constraints: ConstraintSetInput
  def mode: InstrumentModesInput
}

object SpectroscopyTimeInput:
  def unapply(arg: SpectroscopyTimeInput): (
    Wavelength,
    SignalToNoise,
    Option[Wavelength],
    List[TargetDataInput],
    ConstraintSetInput,
    InstrumentModesInput
  ) =
    (
      arg.wavelength,
      arg.signalToNoise,
      arg.signalToNoiseAt,
      arg.asterism,
      arg.constraints,
      arg.mode
    )

case class SpectroscopyIntegrationTimeInput(
  wavelength:      Wavelength,
  signalToNoise:   SignalToNoise,
  signalToNoiseAt: Option[Wavelength],
  asterism:        List[TargetDataInput],
  constraints:     ConstraintSetInput,
  mode:            InstrumentModesInput
) extends SpectroscopyTimeInput

object SpectroscopyIntegrationTimeInput {

  def binding: Matcher[SpectroscopyIntegrationTimeInput] =
    ObjectFieldsBinding.rmap {
      case List(
            WavelengthInput.Binding("wavelength", wavelength),
            SignalToNoiseBinding("signalToNoise", signalToNoise),
            WavelengthInput.Binding.Option("signalToNoiseAt", signalToNoiseAt),
            TargetDataInput.binding.List("asterism", asterism),
            ConstraintSetInput.Binding("constraints", constraints),
            InstrumentModesInput.binding("mode", mode)
          ) =>
        (wavelength, signalToNoise, signalToNoiseAt, asterism, constraints, mode).parMapN(
          apply
        )
    }

}

case class SpectroscopyIntegrationTimeAndGraphsInput(
  wavelength:         Wavelength,
  signalToNoise:      SignalToNoise,
  signalToNoiseAt:    Option[Wavelength],
  asterism:           List[TargetDataInput],
  constraints:        ConstraintSetInput,
  mode:               InstrumentModesInput,
  significantFigures: Option[SignificantFigures]
) extends SpectroscopyTimeInput

object SpectroscopyIntegrationTimeAndGraphsInput {

  def binding: Matcher[SpectroscopyIntegrationTimeAndGraphsInput] =
    ObjectFieldsBinding.rmap {
      case List(
            WavelengthInput.Binding("wavelength", wavelength),
            SignalToNoiseBinding("signalToNoise", signalToNoise),
            WavelengthInput.Binding.Option("signalToNoiseAt", signalToNoiseAt),
            TargetDataInput.binding.List("asterism", asterism),
            ConstraintSetInput.Binding("constraints", constraints),
            InstrumentModesInput.binding("mode", mode),
            SignificantFiguresInput.binding.Option("significantFigures", significantFigures)
          ) =>
        (wavelength,
         signalToNoise,
         signalToNoiseAt,
         asterism,
         constraints,
         mode,
         significantFigures
        ).parMapN(apply)
    }

}
