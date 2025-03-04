// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client.json

import cats.syntax.either.*
import io.circe.*
import io.circe.syntax.*
import lucuma.core.math.Wavelength
import lucuma.core.util.TimeSpan
import lucuma.itc.client.*
import lucuma.core.model.ExposureTimeMode

// Decoders for the client don't need to be as generic as the ones for the server.
object encoders:
  given Encoder[TimeSpan]   = t =>
    Json.obj("microseconds" -> TimeSpan.FromMicroseconds.reverseGet(t).asJson)
  given Encoder[Wavelength] = w => Json.obj("picometers" -> w.toPicometers.value.value.asJson)

  given Encoder[ExposureTimeMode] = {
    case ExposureTimeMode.SignalToNoiseMode(n, w) =>
      Json.obj(
        "signalToNoise" -> Json.obj(
          "value" -> n.asJson,
          "at"    -> w.asJson
        )
      )

    case ExposureTimeMode.TimeAndCountMode(t, c, w) =>
      Json.obj(
        "timeAndCount" -> Json.obj(
          "time"  -> t.asJson,
          "count" -> c.value.asJson,
          "at"    -> w.asJson
        )
      )
  }
