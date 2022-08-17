// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.legacy

import lucuma.itc.search.ObservingMode

case class ItcInstrumentDetails(mode: ObservingMode)

object ItcInstrumentDetails:
  def fromObservingMode(mode: ObservingMode): ItcInstrumentDetails =
    apply(mode)
