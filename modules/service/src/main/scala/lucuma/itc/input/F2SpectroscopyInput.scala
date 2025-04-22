// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.input

import cats.syntax.parallel.*
import lucuma.core.enums.F2Disperser
import lucuma.core.enums.F2Filter
import lucuma.core.enums.F2Fpu
import lucuma.odb.graphql.binding.*

case class F2SpectroscopyInput(
  disperser: F2Disperser,
  filter:    F2Filter,
  fpu:       F2Fpu
) extends InstrumentModesInput

object F2SpectroscopyInput:

  def binding: Matcher[F2SpectroscopyInput] =
    ObjectFieldsBinding.rmap {
      case List(
            F2DisperserBinding("disperser", disperser),
            F2FpuBinding("fpu", fpu),
            F2FilterBinding("filter", filter)
          ) =>
        (disperser, filter, fpu).parMapN((d, f, u) => F2SpectroscopyInput(d, f, u))
    }
