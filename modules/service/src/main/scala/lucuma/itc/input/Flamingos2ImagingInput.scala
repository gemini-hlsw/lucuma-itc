// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.input

import cats.syntax.all.*
import lucuma.core.enums.F2Filter
import lucuma.odb.graphql.binding.*

final case class Flamingos2ImagingInput(
  filter: F2Filter
) extends InstrumentModesInput

object Flamingos2ImagingInput:
  val binding: Matcher[Flamingos2ImagingInput] =
    ObjectFieldsBinding.rmap:
      case List(F2FilterBinding("filter", filter)) =>
        filter.map(f => Flamingos2ImagingInput(f))
