// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client
package arb

import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.*

trait ArbInstrumentMode {

  import ArbEnumerated.*
  import ArbGmosFpu.given

  import InstrumentMode.GmosNorthSpectroscopy
  import InstrumentMode.GmosSouthSpectroscopy

  given Arbitrary[GmosNorthSpectroscopy] =
    Arbitrary {
      for {
        g <- arbitrary[GmosNorthGrating]
        f <- arbitrary[Option[GmosNorthFilter]]
        u <- arbitrary[GmosFpu.North]
      } yield GmosNorthSpectroscopy(g, f, u)
    }

  given Cogen[GmosNorthSpectroscopy] =
    Cogen[
      (
        GmosNorthGrating,
        Option[GmosNorthFilter],
        GmosFpu.North
      )
    ].contramap { a =>
      (
        a.grating,
        a.filter,
        a.fpu
      )
    }

  given Arbitrary[GmosSouthSpectroscopy] =
    Arbitrary {
      for {
        g <- arbitrary[GmosSouthGrating]
        f <- arbitrary[Option[GmosSouthFilter]]
        u <- arbitrary[GmosFpu.South]
      } yield GmosSouthSpectroscopy(g, f, u)
    }

  given Cogen[GmosSouthSpectroscopy] =
    Cogen[
      (
        GmosSouthGrating,
        Option[GmosSouthFilter],
        GmosFpu.South
      )
    ].contramap { a =>
      (
        a.grating,
        a.filter,
        a.fpu
      )
    }

  given Arbitrary[InstrumentMode] =
    Arbitrary {
      Gen.oneOf(
        arbitrary[GmosNorthSpectroscopy],
        arbitrary[GmosSouthSpectroscopy]
      )
    }

  given Cogen[InstrumentMode] =
    Cogen[
      (
        Option[GmosNorthSpectroscopy],
        Option[GmosSouthSpectroscopy]
      )
    ].contramap { a =>
      (
        InstrumentMode.gmosNorth.getOption(a),
        InstrumentMode.gmosSouth.getOption(a)
      )
    }
}

object ArbInstrumentMode extends ArbInstrumentMode
