// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.math.Wavelength
import lucuma.core.math.SignalToNoise
import lucuma.itc.encoders.given
import lucuma.core.enums.Band

case class TargetSignalToNoise(
  wavelength: Wavelength,
  single:     SignalToNoise,
  total:      SignalToNoise,
  bandOrLine: Either[Band, Wavelength]
)

object TargetSignalToNoise:
  given Encoder[TargetSignalToNoise] = t =>
    Json
      .obj(
        "band"         -> t.bandOrLine.left.toOption.asJson,
        "emissionLine" -> t.bandOrLine.toOption.asJson,
        "wavelength"   -> t.wavelength.asJson,
        "single"       -> t.single.asJson,
        "final"        -> t.total.asJson
      )
