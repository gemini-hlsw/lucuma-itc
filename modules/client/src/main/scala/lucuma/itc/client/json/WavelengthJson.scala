// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client.json

import io.circe.*
import io.circe.syntax.*
import lucuma.core.math.Wavelength

implicit val EncoderWavelength: Encoder[Wavelength] =
  (a: Wavelength) =>
    Json.obj(
      "picometers" -> Wavelength.picometers.reverseGet(a).value.asJson
    )

