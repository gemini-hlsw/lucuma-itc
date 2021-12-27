// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.search.syntax

import lucuma.core.enum.PlanetaryNebulaSpectrum

/**
 * Syntax extensions for missing ocs2Tag items
 */
final class PlanetaryNebulaSpectrumOps(val self: PlanetaryNebulaSpectrum) extends AnyVal {

  def ocs2Tag: String =
    self match {
      case PlanetaryNebulaSpectrum.NGC7009 => "O5V"
      case PlanetaryNebulaSpectrum.IC5117  => "O8III"
    }

}

trait ToPlanetaryNebulaSpectrumOps {
  implicit def toPlanetaryNebulaSpectrumOps(
    self: PlanetaryNebulaSpectrum
  ): PlanetaryNebulaSpectrumOps = new PlanetaryNebulaSpectrumOps(self)
}

object planetarynebula extends ToPlanetaryNebulaSpectrumOps
