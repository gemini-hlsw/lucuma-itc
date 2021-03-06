// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.search.syntax

import lucuma.core.enums.GalaxySpectrum

/**
 * Syntax extensions for missing ocs2Tag items
 */
final class GalaxySpectrumOps(val self: GalaxySpectrum) extends AnyVal {

  def ocs2Tag: String =
    self match {
      case GalaxySpectrum.Elliptical => "elliptical-galaxy"
      case GalaxySpectrum.Spiral     => "spiral-galaxy"
    }

}

trait ToGalaxySpectrumOps {
  implicit def toGalaxySpectrumOps(self: GalaxySpectrum): GalaxySpectrumOps =
    new GalaxySpectrumOps(self)
}

object gallaxyspectrum extends ToGalaxySpectrumOps
