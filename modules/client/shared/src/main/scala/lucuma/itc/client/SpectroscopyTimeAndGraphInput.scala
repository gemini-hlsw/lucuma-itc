// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client

import cats.Eq
import cats.data.NonEmptyList
import cats.derived.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Encoder
import io.circe.Json
import io.circe.JsonObject
import io.circe.syntax.*
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.model.ConstraintSet
import lucuma.core.util.TimeSpan
import lucuma.itc.client.json.given
import lucuma.itc.client.json.syntax.*
import lucuma.itc.encoders.given

case class SpectroscopyIntegrationTimeAndGraphsParameters(
  atWavelength:       Wavelength,
  signalToNoise:      SignalToNoise,
  constraints:        ConstraintSet,
  mode:               InstrumentMode,
  significantFigures: Option[SignificantFiguresInput]
) derives Eq

object SpectroscopyIntegrationTimeAndGraphsParameters {
  given Encoder.AsObject[SpectroscopyIntegrationTimeAndGraphsParameters] = a =>
    JsonObject(
      "atWavelength"       -> Json.obj("picometers" -> a.atWavelength.toPicometers.value.asJson),
      "signalToNoise"      -> a.signalToNoise.asJson,
      "constraints"        -> a.constraints.asJson,
      "mode"               -> a.mode.asJson,
      "significantFigures" -> a.significantFigures.asJson
    )
}

case class SpectroscopyIntegrationTimeAndGraphsInput(
  parameters: SpectroscopyIntegrationTimeAndGraphsParameters,
  asterism:   NonEmptyList[TargetInput]
) derives Eq:
  export parameters.*

object SpectroscopyIntegrationTimeAndGraphsInput {
  given Encoder[TimeSpan] = _.toMicroseconds.asJson

  given Encoder.AsObject[SpectroscopyIntegrationTimeAndGraphsInput] = a =>
    JsonObject("asterism" -> a.asterism.asJson).deepMerge(a.parameters.asJsonObject)

}
