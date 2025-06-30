// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.input

import cats.data.NonEmptyList
import cats.syntax.all.*
import grackle.Result
import lucuma.core.model.ExposureTimeMode
import lucuma.itc.SignificantFigures
import lucuma.odb.graphql.binding.*
import lucuma.odb.graphql.input.*

sealed trait SpectroscopyTimeInput:
  def exposureTimeMode: ExposureTimeMode
  def asterism: List[TargetDataInput]
  def constraints: ConstraintSetInput
  def modes: NonEmptyList[InstrumentModesInput]

object SpectroscopyTimeInput:
  def unapply(
    arg: SpectroscopyTimeInput
  ): (ExposureTimeMode,
      List[TargetDataInput],
      ConstraintSetInput,
      NonEmptyList[InstrumentModesInput]
  ) =
    (arg.exposureTimeMode, arg.asterism, arg.constraints, arg.modes)

case class SpectroscopyInput(
  exposureTimeMode: ExposureTimeMode,
  asterism:         List[TargetDataInput],
  constraints:      ConstraintSetInput,
  modes:            NonEmptyList[InstrumentModesInput]
) extends SpectroscopyTimeInput

object SpectroscopyInput:

  val Binding: Matcher[SpectroscopyInput] =
    ObjectFieldsBinding.rmap:
      case List(
            ExposureTimeModeInput.Binding("exposureTimeMode", exposureTimeMode),
            TargetDataInput.Binding.List("asterism", asterism),
            ConstraintSetInput.Binding("constraints", constraints),
            InstrumentModesInput.Binding.List("modes", modes)
          ) =>
        (exposureTimeMode, asterism, constraints, modes)
          .parMapN: (exp, ast, con, modes) =>
            NonEmptyList.fromList(modes) match
              case Some(nel) => Result.success(apply(exp, ast, con, nel))
              case None      => Result.failure("At least one imaging mode is required")
          .flatMap(identity)

case class SpectroscopyIntegrationTimeAndGraphsInput(
  exposureTimeMode:   ExposureTimeMode,
  asterism:           List[TargetDataInput],
  constraints:        ConstraintSetInput,
  mode:               InstrumentModesInput,
  significantFigures: Option[SignificantFigures]
)

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
