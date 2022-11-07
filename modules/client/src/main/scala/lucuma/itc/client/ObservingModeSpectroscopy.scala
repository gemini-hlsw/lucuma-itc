// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client

import cats.Eq
import cats.syntax.either.*
import cats.syntax.eq.*
import cats.syntax.functor.*
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.HCursor
import lucuma.core.enums.Instrument
import lucuma.core.math.Wavelength
import lucuma.itc.client.json.given


final case class ObservingModeSpectroscopy(
  wavelength: Wavelength,
  resolution: BigDecimal,
  params:     InstrumentMode,  // InstrumentITCParams duplicates InstrumentMode so we'll just use that for now
  instrument: Instrument
)

object ObservingModeSpectroscopy {

  given Decoder[ObservingModeSpectroscopy] with
    def apply(c: HCursor): Decoder.Result[ObservingModeSpectroscopy] =
      for {
        w <- c.downField("wavelength").as[Wavelength]
        r <- c.downField("resolution").as[BigDecimal]
        i <- c.downField("instrument").as[Instrument]
        p <- i match {
          case Instrument.GmosNorth =>
            c.downField("params").as[InstrumentMode.GmosNorth].widen[InstrumentMode]
          case Instrument.GmosSouth =>
            c.downField("params").as[InstrumentMode.GmosSouth].widen[InstrumentMode]
          case i                    =>
            DecodingFailure(s"Unexpected / unhandled instrument spectroscopy instrument mode $i.", c.history).asLeft
        }
      } yield ObservingModeSpectroscopy(w, r, p, i)

  given Eq[ObservingModeSpectroscopy] with
    def eqv(x: ObservingModeSpectroscopy, y: ObservingModeSpectroscopy): Boolean =
      (x.wavelength === y.wavelength) &&
        (x.resolution === y.resolution) &&
        (x.params === y.params) &&
        (x.instrument === y.instrument)


}

