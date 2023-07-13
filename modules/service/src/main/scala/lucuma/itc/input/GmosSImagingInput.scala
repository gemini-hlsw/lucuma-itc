// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.input

import cats.syntax.parallel.*
import lucuma.odb.graphql.binding.*
import lucuma.core.enums.GmosSouthFilter

case class GmosSImagingInput(
  filter: GmosSouthFilter
) extends InstrumentModesInput

object GmosSImagingInput {

  def binding: Matcher[GmosSImagingInput] =
    ObjectFieldsBinding.rmap { case List(GmosSouthFilterBinding("filter", filter)) =>
      filter.map(apply)
    }

}
