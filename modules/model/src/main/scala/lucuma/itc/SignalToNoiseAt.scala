// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import cats.Eq
import cats.derived.*
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.util.NewType

object TotalSN extends NewType[SignalToNoise]
type TotalSN = TotalSN.Type

object SingleSN extends NewType[SignalToNoise]
type SingleSN = SingleSN.Type

case class SignalToNoiseAt(
  wavelength: Wavelength,
  single:     SingleSN,
  total:      TotalSN
) derives Eq

object SignalToNoiseAt:
  given (using Encoder[Wavelength]): Encoder[SignalToNoiseAt] = t =>
    Json
      .obj(
        "wavelength" -> t.wavelength.asJson,
        "single"     -> t.single.asJson,
        "total"      -> t.total.asJson
      )
