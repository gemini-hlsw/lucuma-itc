// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import cats.data.NonEmptyList
import coulomb.*
import coulomb.units.si.prefixes.*
import coulomb.units.time.*
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.numeric.PosLong
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.core.util.TimeSpan

type Nanosecond  = Nano * Second
type Microsecond = Micro * Second
type Millisecond = Milli * Second

object encoders:

  given nelEncoder[A: Encoder]: Encoder[NonEmptyList[A]] = Encoder.encodeList[A].contramap(_.toList)
  given Encoder[NonEmptyString]                          = (s: NonEmptyString) => s.value.asJson
  given Encoder[NonNegInt]                               = (s: NonNegInt) => s.value.asJson
  given Encoder[PosInt]                                  = (s: PosInt) => s.value.asJson
  given Encoder[PosLong]                                 = (s: PosLong) => s.value.asJson

  // TODO get this directly from odb schemas
  given Encoder[TimeSpan] =
    Encoder { (ts: TimeSpan) =>
      Json.obj(
        "microseconds" -> TimeSpan.FromMicroseconds.reverseGet(ts).asJson,
        "milliseconds" -> TimeSpan.FromMilliseconds.reverseGet(ts).asJson,
        "seconds"      -> TimeSpan.FromSeconds.reverseGet(ts).asJson,
        "minutes"      -> TimeSpan.FromMinutes.reverseGet(ts).asJson,
        "hours"        -> TimeSpan.FromHours.reverseGet(ts).asJson,
        "iso"          -> TimeSpan.FromString.reverseGet(ts).asJson
      )
    }

  given Encoder[Wavelength] = w =>
    Json.obj(
      ("picometers", Json.fromInt(w.toPicometers.value.value)),
      ("angstroms", Json.fromBigDecimal(w.toAngstroms.value.value)),
      ("nanometers", Json.fromBigDecimal(w.toNanometers.value.value)),
      ("micrometers", Json.fromBigDecimal(w.toMicrometers.value.value))
    )

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
