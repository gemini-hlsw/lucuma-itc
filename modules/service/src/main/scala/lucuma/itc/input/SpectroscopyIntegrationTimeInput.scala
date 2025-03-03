// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.input

import cats.syntax.all.*
import lucuma.core.model.ExposureTimeMode
import lucuma.itc.SignificantFigures
import lucuma.odb.graphql.binding.*
import lucuma.odb.graphql.input.*

sealed trait SpectroscopyTimeInput:
  def exposureTimeMode: ExposureTimeMode
  def asterism: List[TargetDataInput]
  def constraints: ConstraintSetInput
  def mode: InstrumentModesInput

object SpectroscopyTimeInput:
  def unapply(
    arg: SpectroscopyTimeInput
  ): (ExposureTimeMode, List[TargetDataInput], ConstraintSetInput, InstrumentModesInput) =
    (arg.exposureTimeMode, arg.asterism, arg.constraints, arg.mode)

case class SpectroscopyIntegrationTimeInput(
  exposureTimeMode: ExposureTimeMode,
  asterism:         List[TargetDataInput],
  constraints:      ConstraintSetInput,
  mode:             InstrumentModesInput
) extends SpectroscopyTimeInput

object SpectroscopyIntegrationTimeInput:

  val Binding: Matcher[SpectroscopyIntegrationTimeInput] =
    ObjectFieldsBinding.rmap:
      case List(
            ExposureTimeModeInput.Binding("exposureTimeMode", exposureTimeMode),
            TargetDataInput.Binding.List("asterism", asterism),
            ConstraintSetInput.Binding("constraints", constraints),
            InstrumentModesInput.Binding("mode", mode)
          ) =>
        (exposureTimeMode, asterism, constraints, mode)
          .parMapN(apply)

case class SpectroscopyIntegrationTimeAndGraphsInput(
  exposureTimeMode:   ExposureTimeMode,
  asterism:           List[TargetDataInput],
  constraints:        ConstraintSetInput,
  mode:               InstrumentModesInput,
  significantFigures: Option[SignificantFigures]
) extends SpectroscopyTimeInput

object SpectroscopyIntegrationTimeAndGraphsInput:

  val Binding: Matcher[SpectroscopyIntegrationTimeAndGraphsInput] =
    ObjectFieldsBinding.rmap:
      case List(
            ExposureTimeModeInput.Binding("exposureTimeMode", exposureTimeMode),
            TargetDataInput.Binding.List("asterism", asterism),
            ConstraintSetInput.Binding("constraints", constraints),
            InstrumentModesInput.Binding("mode", mode),
            SignificantFiguresInput.Binding.Option("significantFigures", significantFigures)
          ) =>
        (exposureTimeMode, asterism, constraints, mode, significantFigures).parMapN(apply)
