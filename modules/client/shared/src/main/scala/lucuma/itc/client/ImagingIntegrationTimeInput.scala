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

final case class ImagingIntegrationTimeInput(
  wavelength:     Wavelength,
  signalToNoise:  SignalToNoise,
  sourceProfile:  SourceProfile,
  band:           Band,
  radialVelocity: RadialVelocity,
  constraints:    ConstraintSet,
  mode:           InstrumentMode
)

object ImagingIntegrationTimeInput {

  given Encoder[ImagingIntegrationTimeInput] with
    def apply(a: ImagingIntegrationTimeInput): Json =
      Json
        .obj(
          "wavelength"     -> Json.obj("picometers" -> a.wavelength.toPicometers.value.asJson),
          "signalToNoise"  -> a.signalToNoise.asJson,
          "sourceProfile"  -> a.sourceProfile.asJson,
          "band"           -> a.band.asScreamingJson,
          "radialVelocity" -> Json.obj(
            "metersPerSecond" -> RadialVelocity.fromMetersPerSecond
              .reverseGet(a.radialVelocity)
              .asJson
          ),
          "constraints"    -> a.constraints.asJson,
          "mode"           -> a.mode.asJson
        )
        .dropNullValues

  given Eq[ImagingIntegrationTimeInput] =
    Eq.by { a =>
      (
        a.wavelength,
        a.signalToNoise,
        a.sourceProfile,
        a.band,
        a.radialVelocity,
        a.constraints,
        a.mode
      )
    }
}
