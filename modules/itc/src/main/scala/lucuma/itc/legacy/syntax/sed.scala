// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.legacy.syntax

import lucuma.core.enums.CoolStarTemperature
import lucuma.core.enums.GalaxySpectrum
import lucuma.core.enums.HIIRegionSpectrum
import lucuma.core.enums.PlanetSpectrum
import lucuma.core.enums.PlanetaryNebulaSpectrum
import lucuma.core.enums.QuasarSpectrum
import lucuma.core.enums.StellarLibrarySpectrum
import lucuma.core.model.UnnormalizedSED.CoolStarModel

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

/**
 * Syntax extensions for missing ocs2Tag items
 */
trait HIIRegionSpectrumSyntax:
  extension (self: HIIRegionSpectrum)
    def ocs2Tag: String =
      self match
        case HIIRegionSpectrum.OrionNebula => "Orion-nebula"

object hiiregionspectrum extends HIIRegionSpectrumSyntax

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

/**
 * Syntax extensions for missing ocs2Tag items
 */
trait QuasarSpectrumSyntax:
  extension (self: QuasarSpectrum)
    def ocs2Tag: String =
      self match
        case QuasarSpectrum.QS0  => "QSO"
        case QuasarSpectrum.QS02 => "QSO2"

object quasar extends QuasarSpectrumSyntax

/**
 * Syntax extensions for missing ocs2Tag items
 */
trait StellarLibrarySpectrumSyntax:
  extension (self: StellarLibrarySpectrum)
    def ocs2Tag: String =
      self match
        case StellarLibrarySpectrum.O5V     => "O5V"
        case StellarLibrarySpectrum.O8III   => "O8III"
        case StellarLibrarySpectrum.B0V     => "B0V"
        case StellarLibrarySpectrum.B5_7V   => "B5-7V"
        case StellarLibrarySpectrum.B5III   => "B5III"
        case StellarLibrarySpectrum.B5I     => "B5I"
        case StellarLibrarySpectrum.A0V     => "A0V"
        case StellarLibrarySpectrum.A0III   => "A0III"
        case StellarLibrarySpectrum.A0I     => "A0I"
        case StellarLibrarySpectrum.A5V     => "A5V"
        case StellarLibrarySpectrum.A5III   => "A5III"
        case StellarLibrarySpectrum.F0V     => "F0V"
        case StellarLibrarySpectrum.F0III   => "F0III"
        case StellarLibrarySpectrum.F0I     => "F0I"
        case StellarLibrarySpectrum.F5V     => "F5V"
        case StellarLibrarySpectrum.F5V_w   => "F5V-w"
        case StellarLibrarySpectrum.F6V_r   => "F6V-r"
        case StellarLibrarySpectrum.F5III   => "F5III"
        case StellarLibrarySpectrum.F5I     => "F5I"
        case StellarLibrarySpectrum.G0V     => "G0V"
        case StellarLibrarySpectrum.G0V_w   => "G0V-w"
        case StellarLibrarySpectrum.G0V_r   => "G0V-r"
        case StellarLibrarySpectrum.G0III   => "G0III"
        case StellarLibrarySpectrum.G0I     => "G0I"
        case StellarLibrarySpectrum.G2V     => "G2V"
        case StellarLibrarySpectrum.G5V     => "G5V"
        case StellarLibrarySpectrum.G5V_w   => "G5V-w"
        case StellarLibrarySpectrum.G5V_r   => "G5V-r"
        case StellarLibrarySpectrum.G5III   => "G5III"
        case StellarLibrarySpectrum.G5III_w => "G5III-w"
        case StellarLibrarySpectrum.G5III_r => "G5III-r"
        case StellarLibrarySpectrum.G5I     => "G5I"
        case StellarLibrarySpectrum.K0V     => "K0V"
        case StellarLibrarySpectrum.K0V_r   => "K0V-r"
        case StellarLibrarySpectrum.K0III   => "K0III"
        case StellarLibrarySpectrum.K0III_w => "K0III-w"
        case StellarLibrarySpectrum.K0III_r => "K0III-r"
        case StellarLibrarySpectrum.K0_1II  => "K0-1II"
        case StellarLibrarySpectrum.K4V     => "K4V"
        case StellarLibrarySpectrum.K4III   => "K4III"
        case StellarLibrarySpectrum.K4III_w => "K4III-w"
        case StellarLibrarySpectrum.K4III_r => "K4III-r"
        case StellarLibrarySpectrum.K4I     => "K4I"
        case StellarLibrarySpectrum.M0V     => "M0V"
        case StellarLibrarySpectrum.M0III   => "M0III"
        case StellarLibrarySpectrum.M3V     => "M3V"
        case StellarLibrarySpectrum.M3III   => "M3III"
        case StellarLibrarySpectrum.M6V     => "M6V"
        case StellarLibrarySpectrum.M6III   => "M6III"
        case StellarLibrarySpectrum.M9III   => "M9III"

object stellarLibrary extends StellarLibrarySpectrumSyntax
