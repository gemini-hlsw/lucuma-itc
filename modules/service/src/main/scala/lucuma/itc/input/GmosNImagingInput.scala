// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.input

import cats.syntax.parallel.*
import lucuma.core.enums.GmosNorthFilter
import lucuma.odb.graphql.binding.*

case class GmosNImagingInput(
  filter: GmosNorthFilter
) extends InstrumentModesInput

object GmosNImagingInput {

  def binding: Matcher[GmosNImagingInput] =
    ObjectFieldsBinding.rmap { case List(GmosNorthFilterBinding("filter", filter)) =>
      filter.map(apply)
    }

}
