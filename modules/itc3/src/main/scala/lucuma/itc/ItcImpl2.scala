// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

object repro {
  import coulomb.*
  import coulomb.syntax.*

  import algebra.instances.all.given
  import coulomb.ops.algebra.spire.all.given

  import coulomb.policy.spire.standard.given
  import coulomb.units.si.*
  import coulomb.units.si.given
  import scala.math._

  val expTime: Quantity[BigDecimal, Second]  = BigDecimal(1).withUnit[Second]
  val nExp                                   = 1
  val expTime2: Quantity[BigDecimal, Second] = BigDecimal(1).withUnit[Second]
  expTime + expTime2
}
