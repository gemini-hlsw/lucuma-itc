// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.input

import cats.syntax.parallel.*
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.model.sequence.gmos.GmosFpuMask
import lucuma.odb.graphql.binding.*
import lucuma.odb.graphql.input.*

case class GmosSSpectroscopyInput(
  grating: GmosSouthGrating,
  fpu:     GmosFpuMask[GmosSouthFpu],
  filter:  Option[GmosSouthFilter]
) extends InstrumentModesInput

object GmosSSpectroscopyInput {

  def binding: Matcher[GmosSSpectroscopyInput] =
    ObjectFieldsBinding.rmap {
      case List(
            GmosSouthGratingBinding("grating", grating),
            GmosSouthFpuInput.Binding("fpu", fpu),
            GmosSouthFilterBinding.Option("filter", filter)
          ) =>
        (grating, fpu, filter).parMapN(apply)
    }

}
