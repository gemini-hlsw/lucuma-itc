// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.legacy.syntax

import lucuma.core.enums.PlanetaryNebulaSpectrum

/**
 * Syntax extensions for missing ocs2Tag items
 */
trait PlanetaryNebulaSpectrumSyntax:
  extension (self: PlanetaryNebulaSpectrum)
    def ocs2Tag: String =
      self match
        case PlanetaryNebulaSpectrum.NGC7009 => "O5V"
        case PlanetaryNebulaSpectrum.IC5117  => "O8III"

object planetarynebula extends PlanetaryNebulaSpectrumSyntax
