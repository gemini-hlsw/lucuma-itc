// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.input

import cats.data.NonEmptyList
import cats.syntax.all.*
import grackle.Result
import lucuma.core.model.ExposureTimeMode
import lucuma.odb.graphql.binding.*
import lucuma.odb.graphql.input.*

case class ImagingInput(
  exposureTimeMode: ExposureTimeMode,
  asterism:         List[TargetDataInput],
  constraints:      ConstraintSetInput,
  modes:            NonEmptyList[InstrumentModesInput]
)

object ImagingInput:

  val Binding: Matcher[ImagingInput] =
    ObjectFieldsBinding.rmap:
      case List(
            ExposureTimeModeInput.Binding("exposureTimeMode", exposureTimeMode),
            TargetDataInput.Binding.List("asterism", asterism),
            ConstraintSetInput.Binding("constraints", constraints),
            InstrumentModesInput.Binding.List("modes", modes)
          ) =>
        (exposureTimeMode, asterism, constraints, modes)
          .parFlatMapN: (exp, ast, con, modes) =>
            NonEmptyList.fromList(modes) match
              case Some(nel) => Result.success(apply(exp, ast, con, nel))
              case None      => Result.failure("At least one imaging mode is required")
