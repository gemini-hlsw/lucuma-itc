// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import coulomb.*
import coulomb.policy.spire.standard.given
import coulomb.syntax.*
import coulomb.units.si.prefixes.*
import coulomb.units.time.*
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.math.Wavelength

import java.math.RoundingMode
import scala.concurrent.duration.FiniteDuration

type Nanosecond  = Nano * Second
type Microsecond = Micro * Second
type Millisecond = Milli * Second

object encoders:

  given Encoder[NonEmptyString] = (s: NonEmptyString) => s.value.asJson
  given Encoder[PosInt]         = (s: PosInt) => s.value.asJson

  given Encoder[FiniteDuration] = d =>
    val value: Quantity[Long, Nanosecond] = d.toNanos.withUnit[Nanosecond]

    Json.obj(
      ("microseconds", Json.fromLong(value.tToUnit[Microsecond].value)),
      ("milliseconds", Json.fromBigDecimal(value.toValue[BigDecimal].toUnit[Millisecond].value)),
      ("seconds", Json.fromBigDecimal(value.toValue[BigDecimal].toUnit[Second].value)),
      ("minutes", Json.fromBigDecimal(value.toValue[BigDecimal].toUnit[Minute].value)),
      ("hours", Json.fromBigDecimal(value.toValue[BigDecimal].toUnit[Hour].value))
    )

  given Encoder[Wavelength] = w =>
    Json.obj(
      ("picometers",  Json.fromInt(w.toPicometers.value.value)),
      ("angstrom",    Json.fromBigDecimal(w.toAngstroms.value.value)),
      ("nanometers",  Json.fromBigDecimal(w.toNanometers.value.value)),
      ("micrometers", Json.fromBigDecimal(w.toMicrometers.value.value))
    )
