// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.input

import cats.syntax.parallel.*
import lucuma.core.math.Wavelength
import lucuma.core.enums.Band
import lucuma.core.model.SourceProfile
import lucuma.core.math.RadialVelocity
import lucuma.odb.graphql.binding.*
import lucuma.odb.graphql.input.*
import lucuma.core.util.TimeSpan
import lucuma.odb.graphql.input.sourceprofile.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.itc.SignificantFigures

case class OptimizedSpectroscopyGraphInput(
  wavelength:         Wavelength,
  signalToNoiseAt:    Option[Wavelength],
  exposureTime:       TimeSpan,
  exposures:          PosInt,
  sourceProfile:      SourceProfile,
  band:               Band,
  radialVelocity:     RadialVelocity,
  constraints:        ConstraintSetInput,
  mode:               InstrumentModesInput,
  significantFigures: Option[SignificantFigures]
)

object OptimizedSpectroscopyGraphInput {

  def binding: Matcher[OptimizedSpectroscopyGraphInput] =
    ObjectFieldsBinding.rmap {
      case List(
            WavelengthInput.Binding("wavelength", wavelength),
            WavelengthInput.Binding.Option("signalToNoiseAt", signalToNoiseAt),
            TimeSpanInput.Binding("exposureTime", exposureTime),
            PosIntBinding("exposures", exposures),
            SourceProfileInput.CreateBinding("sourceProfile", sourceProfile),
            BandBinding("band", band),
            RadialVelocityInput.Binding("radialVelocity", radialVelocity),
            ConstraintSetInput.Binding("constraints", constraints),
            InstrumentModesInput.binding("mode", mode),
            SignificantFiguresInput.binding.Option("significantFigures", significantFigures)
          ) =>
        (wavelength,
         signalToNoiseAt,
         exposureTime,
         exposures,
         sourceProfile,
         band,
         radialVelocity,
         constraints,
         mode,
         significantFigures
        ).parMapN(apply)
    }

}
