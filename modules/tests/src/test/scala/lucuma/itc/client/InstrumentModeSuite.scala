// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client

import cats.kernel.laws.discipline.*
import monocle.law.discipline.*
import munit.*
import lucuma.itc.client.arb.ArbInstrumentMode

final class InstrumentModeSuite extends DisciplineSuite {

  import ArbInstrumentMode.given

  checkAll("InstrumentMode", EqTests[InstrumentMode].eqv)
  checkAll("InstrumentMode.gmosNorth", PrismTests(InstrumentMode.gmosNorth))

}
