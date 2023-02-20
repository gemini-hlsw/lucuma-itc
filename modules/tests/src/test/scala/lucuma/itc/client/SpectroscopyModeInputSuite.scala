// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client

import cats.kernel.laws.discipline.*
import monocle.law.discipline._
import munit._
import lucuma.itc.client.arb.ArbSpectroscopyModeInput

final class SpectroscopyModeInputSuite extends DisciplineSuite {

  import ArbSpectroscopyModeInput.given

  checkAll("SpectroscopyModeInput", EqTests[SpectroscopyModeInput].eqv)

}
