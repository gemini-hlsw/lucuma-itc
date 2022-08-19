// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.legacy.syntax

import lucuma.core.enums.PlanetSpectrum

/**
 * Syntax extensions for missing ocs2Tag items
 */
trait PlanetSpectrumSyntax:
  extension (self: PlanetSpectrum)
    def ocs2Tag: String =
      self match
        case PlanetSpectrum.Mars    => "Mars"
        case PlanetSpectrum.Jupiter => "Jupiter"
        case PlanetSpectrum.Saturn  => "Saturn"
        case PlanetSpectrum.Uranus  => "Uranus"
        case PlanetSpectrum.Neptune => "Neptune"

object planets extends PlanetSpectrumSyntax
