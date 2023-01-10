// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client

import eu.timepit.refined.types.numeric.PosBigDecimal
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.Band
import lucuma.core.math.RadialVelocity
import lucuma.core.math.Wavelength
import lucuma.core.model.ConstraintSet
import lucuma.core.model.SourceProfile
import lucuma.itc.client.json.given
import lucuma.itc.client.json.syntax.*

final case class SpectroscopyModeInput(
  wavelength:      Wavelength,
  signalToNoise:   PosBigDecimal,
  signalToNoiseAt: Option[Wavelength],
  sourceProfile:   SourceProfile,
  band:            Band,
  radialVelocity:  RadialVelocity,
  constraints:     ConstraintSet,
  mode:            InstrumentMode
)

object SpectroscopyModeInput {

  given Encoder[SpectroscopyModeInput] with
    def apply(a: SpectroscopyModeInput): Json =
      Json
        .obj(
          "wavelength"      -> a.wavelength.asJson,
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
          "modes"           -> List(a.mode).asJson
        )
        .dropNullValues

}
