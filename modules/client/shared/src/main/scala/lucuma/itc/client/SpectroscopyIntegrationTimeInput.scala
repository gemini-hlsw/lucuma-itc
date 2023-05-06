// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client

import cats.Eq
import eu.timepit.refined.cats.*
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.Band
import lucuma.core.math.RadialVelocity
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.model.ConstraintSet
import lucuma.core.model.SourceProfile
import lucuma.itc.client.json.given
import lucuma.itc.client.json.syntax.*
import lucuma.itc.encoders.given

final case class SpectroscopyIntegrationTimeInput(
  wavelength:      Wavelength,
  signalToNoise:   SignalToNoise,
  signalToNoiseAt: Option[Wavelength],
  sourceProfile:   SourceProfile,
  band:            Band,
  radialVelocity:  RadialVelocity,
  constraints:     ConstraintSet,
  mode:            InstrumentMode
)

object SpectroscopyIntegrationTimeInput {

  given Encoder[SpectroscopyIntegrationTimeInput] with
    def apply(a: SpectroscopyIntegrationTimeInput): Json =
      Json
        .obj(
          "wavelength"      -> Json.obj("picometers" -> a.wavelength.toPicometers.value.asJson),
          "signalToNoise"   -> a.signalToNoise.asJson,
          "signalToNoiseAt" -> a.signalToNoiseAt.asJson,
          "sourceProfile"   -> a.sourceProfile.asJson,
          "band"            -> a.band.asScreamingJson,
          "radialVelocity"  -> Json.obj(
            "metersPerSecond" -> RadialVelocity.fromMetersPerSecond
              .reverseGet(a.radialVelocity)
              .asJson
          ),
          "constraints"     -> a.constraints.asJson,
          "mode"            -> a.mode.asJson
        )
        .dropNullValues

  given Eq[SpectroscopyIntegrationTimeInput] =
    Eq.by { a =>
      (
        a.wavelength,
        a.signalToNoise,
        a.signalToNoiseAt,
        a.sourceProfile,
        a.band,
        a.radialVelocity,
        a.constraints,
        a.mode
      )
    }
}
