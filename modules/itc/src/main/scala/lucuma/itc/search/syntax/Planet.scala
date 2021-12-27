// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.search.syntax

import lucuma.core.enum.PlanetSpectrum

/**
 * Syntax extensions for missing ocs2Tag items
 */
final class PlanetSpectrumOps(val self: PlanetSpectrum) extends AnyVal {

  def ocs2Tag: String =
    self match {
      case PlanetSpectrum.Mars    => "Mars"
      case PlanetSpectrum.Jupiter => "Jupiter"
      case PlanetSpectrum.Saturn  => "Saturn"
      case PlanetSpectrum.Uranus  => "Uranus"
      case PlanetSpectrum.Neptune => "Neptune"
    }

}

trait ToPlanetSpectrumOps {
  implicit def toPlanetSpectrumOps(self: PlanetSpectrum): PlanetSpectrumOps = new PlanetSpectrumOps(
    self
  )
}

object planets extends ToPlanetSpectrumOps
