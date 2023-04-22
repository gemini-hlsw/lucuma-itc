// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client

import cats.kernel.laws.discipline.*
import lucuma.itc.client.arb.ArbSpectroscopyIntegrationTimeInput
import monocle.law.discipline._
import munit._

final class SpectroscopyIntegrationTimeInputSuite extends DisciplineSuite {

  import ArbSpectroscopyIntegrationTimeInput.given

  checkAll("SpectroscopyIntegrationTimeInput", EqTests[SpectroscopyIntegrationTimeInput].eqv)

}
