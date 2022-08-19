// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.legacy.syntax

import lucuma.core.enums.CoolStarTemperature
import lucuma.core.model.UnnormalizedSED.CoolStarModel

/**
 * Syntax extensions for missing ocs2Tag items
 */
trait CoolStarModelSyntax:
  extension (self: CoolStarModel)
    def ocs2Tag: String = self.temperature match
      case CoolStarTemperature.T400K => "T0400K"
      case CoolStarTemperature.T600K => "T0600K"
      case CoolStarTemperature.T800K => "T0800K"
      case CoolStarTemperature.T900K => "T0900K"
      case _                         => s"${self.temperature}"

object coolstar extends CoolStarModelSyntax
