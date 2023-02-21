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

  import InstrumentMode.GmosNorth
  import InstrumentMode.GmosSouth

  given Arbitrary[GmosNorth] =
    Arbitrary {
      for {
        g <- arbitrary[GmosNorthGrating]
        f <- arbitrary[Option[GmosNorthFilter]]
        u <- arbitrary[GmosFpu.North]
      } yield GmosNorth(g, f, u)
    }

  given Cogen[GmosNorth] =
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

  given Arbitrary[GmosSouth] =
    Arbitrary {
      for {
        g <- arbitrary[GmosSouthGrating]
        f <- arbitrary[Option[GmosSouthFilter]]
        u <- arbitrary[GmosFpu.South]
      } yield GmosSouth(g, f, u)
    }

  given Cogen[GmosSouth] =
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
        arbitrary[GmosNorth],
        arbitrary[GmosSouth]
      )
    }

  given Cogen[InstrumentMode] =
    Cogen[
      (
        Option[GmosNorth],
        Option[GmosSouth]
      )
    ].contramap { a =>
      (
        InstrumentMode.gmosNorth.getOption(a),
        InstrumentMode.gmosSouth.getOption(a)
      )
    }
}

object ArbInstrumentMode extends ArbInstrumentMode
