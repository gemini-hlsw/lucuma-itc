// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.legacy.syntax

import lucuma.core.enums.HIIRegionSpectrum

/**
 * Syntax extensions for missing ocs2Tag items
 */
trait HIIRegionSpectrumSyntax:
  extension (self: HIIRegionSpectrum)
    def ocs2Tag: String =
      self match
        case HIIRegionSpectrum.OrionNebula => "Orion-nebula"

object hiiregionspectrum extends HIIRegionSpectrumSyntax
