// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.input

import cats.syntax.parallel.*
import lucuma.core.model.ExposureTimeMode
import lucuma.odb.graphql.binding.*
import lucuma.odb.graphql.input.*

case class ImagingIntegrationTimeInput(
  exposureTimeMode: ExposureTimeMode,
  asterism:         List[TargetDataInput],
  constraints:      ConstraintSetInput,
  mode:             InstrumentModesInput
)

object ImagingIntegrationTimeInput:

  def binding: Matcher[ImagingIntegrationTimeInput] =
    ObjectFieldsBinding.rmap {
      case List(
            ExposureTimeModeInput.Binding("exposureTimeMode", exposureTimeMode),
            TargetDataInput.binding.List("asterism", asterism),
            ConstraintSetInput.Binding("constraints", constraints),
            InstrumentModesInput.binding("mode", mode)
          ) =>
        (exposureTimeMode, asterism, constraints, mode).parMapN(apply)
    }
