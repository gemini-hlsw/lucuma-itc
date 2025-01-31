// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.input

import cats.syntax.parallel.*
import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.math.Wavelength
import lucuma.core.util.TimeSpan
import lucuma.itc.SignificantFigures
import lucuma.odb.graphql.binding.*
import lucuma.odb.graphql.input.*

case class SpectroscopyGraphsInput(
  atWavelength:       Wavelength,
  exposureTime:       TimeSpan,
  exposureCount:      NonNegInt,
  asterism:           List[TargetDataInput],
  constraints:        ConstraintSetInput,
  mode:               InstrumentModesInput,
  significantFigures: Option[SignificantFigures]
)

object SpectroscopyGraphsInput {

  def binding: Matcher[SpectroscopyGraphsInput] =
    ObjectFieldsBinding.rmap {
      case List(
            WavelengthInput.Binding("atWavelength", wavelength),
            TimeSpanInput.Binding("exposureTime", exposureTime),
            NonNegIntBinding("exposureCount", exposureCount),
            TargetDataInput.binding.List("asterism", asterism),
            ConstraintSetInput.Binding("constraints", constraints),
            InstrumentModesInput.binding("mode", mode),
            SignificantFiguresInput.binding.Option("significantFigures", significantFigures)
          ) =>
        (wavelength, exposureTime, exposureCount, asterism, constraints, mode, significantFigures)
          .parMapN(apply)
    }

}
