// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client

import cats.kernel.laws.discipline.*
import lucuma.itc.client.arb.ArbInstrumentMode
import monocle.law.discipline.*
import munit.*

final class InstrumentModeSuite extends DisciplineSuite {

  import ArbInstrumentMode.given

  checkAll("InstrumentMode", EqTests[InstrumentMode].eqv)
  checkAll("InstrumentMode.gmosNorth", PrismTests(InstrumentMode.gmosNorth))
  checkAll("InstrumentMode.gmosSouth", PrismTests(InstrumentMode.gmosSouth))

}
