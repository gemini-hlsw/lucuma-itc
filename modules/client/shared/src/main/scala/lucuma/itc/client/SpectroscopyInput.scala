// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client

import cats.Eq
import cats.data.NonEmptyList
import cats.derived.*
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ExposureTimeMode
import lucuma.itc.client.json.given
import lucuma.itc.client.json.syntax.*
import lucuma.itc.client.json.encoders.given

case class SpectroscopyParameters(
  exposureTimeMode: ExposureTimeMode,
  constraints:      ConstraintSet,
  mode:             InstrumentMode
) derives Eq

object SpectroscopyParameters {
  given Encoder[SpectroscopyParameters] with
    def apply(a: SpectroscopyParameters): Json =
      Json
        .obj(
          "exposureTimeMode" -> a.exposureTimeMode.asJson,
          "constraints"      -> a.constraints.asJson,
          "mode"             -> a.mode.asJson
        )
        .dropNullValues
}

case class SpectroscopyInput(
  parameters: SpectroscopyParameters,
  asterism:   NonEmptyList[TargetInput]
) derives Eq:
  export parameters.*

object SpectroscopyInput {
  given Encoder[SpectroscopyInput] with
    def apply(a: SpectroscopyInput): Json =
      Json.obj("asterism" -> a.asterism.asJson).deepMerge(a.parameters.asJson)
}
