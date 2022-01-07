// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.search.syntax

import lucuma.core.enum.HIIRegionSpectrum

/**
 * Syntax extensions for missing ocs2Tag items
 */
final class HIIRegionSpectrumOps(val self: HIIRegionSpectrum) extends AnyVal {

  def ocs2Tag: String =
    self match {
      case HIIRegionSpectrum.OrionNebula => "Orion-nebula"
    }

}

trait ToHIIRegionSpectrumOps {
  implicit def toHIIRegionSpectrumOps(self: HIIRegionSpectrum): HIIRegionSpectrumOps =
    new HIIRegionSpectrumOps(self)
}

object hiiregionspectrum extends ToHIIRegionSpectrumOps
