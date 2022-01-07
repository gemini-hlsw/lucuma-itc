// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.search.syntax

import lucuma.core.model.UnnormalizedSED.CoolStarModel
import lucuma.core.enum.CoolStarTemperature

/**
 * Syntax extensions for missing ocs2Tag items
 */
final class CoolStarModelOps(val self: CoolStarModel) extends AnyVal {

  def ocs2Tag: String = self.temperature match {
    case CoolStarTemperature.T400K => "T0400K"
    case CoolStarTemperature.T600K => "T0600K"
    case CoolStarTemperature.T800K => "T0800K"
    case CoolStarTemperature.T900K => "T0900K"
    case _                         => s"${self.temperature}"
  }

}

trait ToCoolStarModelOps {
  implicit def toCoolStarModelOps(self: CoolStarModel): CoolStarModelOps = new CoolStarModelOps(
    self
  )
}

object coolstar extends ToCoolStarModelOps
