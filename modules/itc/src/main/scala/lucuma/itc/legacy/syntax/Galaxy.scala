// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.legacy.syntax

import lucuma.core.enums.GalaxySpectrum

/**
 * Syntax extensions for missing ocs2Tag items
 */
trait GalaxySpectrumSyntax:
  extension (self: GalaxySpectrum)
    def ocs2Tag: String =
      self match
        case GalaxySpectrum.Elliptical => "elliptical-galaxy"
        case GalaxySpectrum.Spiral     => "spiral-galaxy"

object gallaxyspectrum extends GalaxySpectrumSyntax
