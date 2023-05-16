// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client

import cats.Eq
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Encoder
import io.circe.Json
import io.circe.JsonObject
import io.circe.syntax.*
import lucuma.core.enums.Band
import lucuma.core.math.RadialVelocity
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.model.ConstraintSet
import lucuma.core.model.NonNegDuration
import lucuma.core.model.SourceProfile
import lucuma.core.model.given
import lucuma.core.util.TimeSpan
import lucuma.itc.client.json.given
import lucuma.itc.client.json.syntax.*
import lucuma.itc.encoders.given

final case class OptimizedSpectroscopyGraphInput(
  wavelength:         Wavelength,
  signalToNoiseAt:    Option[Wavelength],
  exposureTime:       TimeSpan,
  exposures:          PosInt,
  sourceProfile:      SourceProfile,
  band:               Band,
  radialVelocity:     RadialVelocity,
  constraints:        ConstraintSet,
  mode:               InstrumentMode,
  significantFigures: Option[SignificantFiguresInput]
)

object OptimizedSpectroscopyGraphInput {
  given Encoder[TimeSpan] = _.toMicroseconds.asJson

  given Encoder.AsObject[OptimizedSpectroscopyGraphInput] = a =>
    JsonObject(
      "wavelength"         -> Json.obj("picometers" -> a.wavelength.toPicometers.value.asJson),
      "signalToNoiseAt"    -> a.signalToNoiseAt
        .map(w => Json.obj("picometers" -> w.toPicometers.value.asJson))
        .asJson,
      "exposureTime"       -> Json.obj("microseconds" -> a.exposureTime.asJson),
      "exposures"          -> a.exposures.value.asJson,
      "sourceProfile"      -> a.sourceProfile.asJson,
      "band"               -> a.band.asScreamingJson,
      "radialVelocity"     -> Json.obj(
        "metersPerSecond" -> RadialVelocity.fromMetersPerSecond
          .reverseGet(a.radialVelocity)
          .asJson
      ),
      "constraints"        -> a.constraints.asJson,
      "mode"               -> a.mode.asJson,
      "significantFigures" -> a.significantFigures.asJson
    )

  given Eq[OptimizedSpectroscopyGraphInput] =
    Eq.by { a =>
      (
        a.wavelength,
        a.signalToNoiseAt,
        a.exposureTime,
        a.exposures,
        a.sourceProfile,
        a.band,
        a.radialVelocity,
        a.constraints,
        a.mode,
        a.significantFigures
      )
    }
}
