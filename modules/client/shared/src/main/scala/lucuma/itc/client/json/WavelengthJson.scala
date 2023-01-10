// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client.json

import cats.syntax.either.*
import cats.syntax.option.*
import io.circe.*
import io.circe.syntax.*
import lucuma.core.math.Wavelength

given Encoder[Wavelength] with
  def apply(a: Wavelength): Json =
    Json.obj(
      "picometers" -> Wavelength.picometers.reverseGet(a).value.asJson
    )

given Decoder[Wavelength] with
  def apply(c: HCursor): Decoder.Result[Wavelength] =
    c.downField("picometers").as[Int].flatMap { pm =>
      Wavelength.intPicometers
        .getOption(pm)
        .toRight(
          DecodingFailure("Expected positive integer wavelength value for 'picometers'.", c.history)
        )
    }
