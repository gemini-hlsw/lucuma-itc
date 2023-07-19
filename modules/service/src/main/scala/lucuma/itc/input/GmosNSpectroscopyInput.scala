// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.input

import cats.syntax.parallel.*
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.model.sequence.gmos.GmosFpuMask
import lucuma.odb.graphql.binding.*
import lucuma.odb.graphql.input.*

case class GmosNSpectroscopyInput(
  grating: GmosNorthGrating,
  fpu:     GmosFpuMask[GmosNorthFpu],
  filter:  Option[GmosNorthFilter]
) extends InstrumentModesInput

object GmosNSpectroscopyInput {

  def binding: Matcher[GmosNSpectroscopyInput] =
    ObjectFieldsBinding.rmap {
      case List(
            GmosNorthGratingBinding("grating", grating),
            GmosNorthFpuInput.Binding("fpu", fpu),
            GmosNorthFilterBinding.Option("filter", filter)
          ) =>
        (grating, fpu, filter).parMapN(apply)
    }

}
