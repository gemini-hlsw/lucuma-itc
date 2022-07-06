// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import coulomb.*
import coulomb.ops.algebra.spire.all.given
import coulomb.policy.spire.standard.given
import coulomb.syntax.*
import coulomb.units.si.*
import coulomb.units.si.given
import coulomb.units.si.prefixes.*
import coulomb.units.time.*
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.*
import io.circe.generic.semiauto._
import io.circe.syntax.*
import lucuma.core.enums._
import lucuma.core.math.Wavelength
import lucuma.itc.search.ObservingMode.Spectroscopy._
import lucuma.itc.search.*

import java.math.RoundingMode
import scala.concurrent.duration.FiniteDuration

type Nanosecond  = Nano * Second
type Microsecond = Micro * Second
type Millisecond = Milli * Second

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

given Encoder[Wavelength] = new Encoder[Wavelength]:
  def apply(w: Wavelength): Json = Json.obj(
    ("picometers", Json.fromInt(w.toPicometers.value.value)),
    ("angstrom", Json.fromBigDecimal(w.angstrom.value.toBigDecimal(2, RoundingMode.CEILING))),
    ("nanometers",
     Json.fromBigDecimal(
       w.nanometer.value.toBigDecimal(2, RoundingMode.CEILING)
     )
    ),
    ("micrometers",
     Json.fromBigDecimal(
       w.micrometer.value.toBigDecimal(2, RoundingMode.CEILING)
     )
    )
  )

given Encoder[GmosNorth] = new Encoder[GmosNorth]:
  def apply(a: GmosNorth): Json = Json.obj(
    ("instrument", Json.fromString(a.instrument.longName.toUpperCase.replace(" ", "_"))),
    ("resolution", Json.fromInt(a.resolution.toInt)),
    ("params", GmosNITCParams(a.disperser, a.fpu, a.filter).asJson),
    ("wavelength", a.λ.asJson)
  )

given Encoder[GmosSouth] = new Encoder[GmosSouth]:
  def apply(a: GmosSouth): Json = Json.obj(
    ("instrument", Json.fromString(a.instrument.longName.toUpperCase.replace(" ", "_"))),
    ("resolution", Json.fromInt(a.resolution.toInt)),
    ("params", GmosSITCParams(a.disperser, a.fpu, a.filter).asJson),
    ("wavelength", a.λ.asJson)
  )

given Encoder[ObservingMode.Spectroscopy] = Encoder.instance {
  case gn: GmosNorth => gn.asJson
  case gs: GmosSouth => gs.asJson
}

given Encoder[Itc.Result] = Encoder.instance {
  case f: Itc.Result.Success          =>
    Json.obj(("resultType", Json.fromString("Success"))).deepMerge(f.asJson)
  case Itc.Result.CalculationError(m) =>
    Json.obj(("resultType", Json.fromString("Error")), ("msg", Json.fromString(m)))
  case Itc.Result.SourceTooBright(m)  =>
    Json.obj(("resultType", Json.fromString("Error")),
             ("msg", Json.fromString(s"Source too bright $m"))
    )
}
