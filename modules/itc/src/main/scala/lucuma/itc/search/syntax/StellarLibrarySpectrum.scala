// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.search.syntax

import lucuma.core.enum.StellarLibrarySpectrum

/**
 * Syntax extensions for missing ocs2Tag items
 */
final class StellarLibrarySpectrumOps(val self: StellarLibrarySpectrum) extends AnyVal {

  def ocs2Tag: String =
    self match {
      case StellarLibrarySpectrum.O5V     => "O5V"
      case StellarLibrarySpectrum.O8III   => "O8III"
      case StellarLibrarySpectrum.B0V     => "B0V"
      case StellarLibrarySpectrum.B5_7V   => "B5_7V"
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
      case StellarLibrarySpectrum.F5V_w   => "F5V_w"
      case StellarLibrarySpectrum.F6V_r   => "F6V_r"
      case StellarLibrarySpectrum.F5III   => "F5III"
      case StellarLibrarySpectrum.F5I     => "F5I"
      case StellarLibrarySpectrum.G0V     => "G0V"
      case StellarLibrarySpectrum.G0V_w   => "G0V_w"
      case StellarLibrarySpectrum.G0V_r   => "G0V_r"
      case StellarLibrarySpectrum.G0III   => "G0III"
      case StellarLibrarySpectrum.G0I     => "G0I"
      case StellarLibrarySpectrum.G2V     => "G2V"
      case StellarLibrarySpectrum.G5V     => "G5V"
      case StellarLibrarySpectrum.G5V_w   => "G5V_w"
      case StellarLibrarySpectrum.G5V_r   => "G5V_r"
      case StellarLibrarySpectrum.G5III   => "G5III"
      case StellarLibrarySpectrum.G5III_w => "G5III_w"
      case StellarLibrarySpectrum.G5III_r => "G5III_r"
      case StellarLibrarySpectrum.G5I     => "G5I"
      case StellarLibrarySpectrum.K0V     => "K0V"
      case StellarLibrarySpectrum.K0V_r   => "K0V_r"
      case StellarLibrarySpectrum.K0III   => "K0III"
      case StellarLibrarySpectrum.K0III_w => "K0III_w"
      case StellarLibrarySpectrum.K0III_r => "K0III_r"
      case StellarLibrarySpectrum.K0_1II  => "K0_1II"
      case StellarLibrarySpectrum.K4V     => "K4V"
      case StellarLibrarySpectrum.K4III   => "K4III"
      case StellarLibrarySpectrum.K4III_w => "K4III_w"
      case StellarLibrarySpectrum.K4III_r => "K4III_r"
      case StellarLibrarySpectrum.K4I     => "K4I"
      case StellarLibrarySpectrum.M0V     => "M0V"
      case StellarLibrarySpectrum.M0III   => "M0III"
      case StellarLibrarySpectrum.M3V     => "M3V"
      case StellarLibrarySpectrum.M3III   => "M3III"
      case StellarLibrarySpectrum.M6V     => "M6V"
      case StellarLibrarySpectrum.M6III   => "M6III"
      case StellarLibrarySpectrum.M9III   => "M9III"
    }

}

trait ToStellarLibrarySpectrumOps {
  implicit def toStellarLibrarySpectrumOps(
    self: StellarLibrarySpectrum
  ): StellarLibrarySpectrumOps =
    new StellarLibrarySpectrumOps(self)
}

object stellarLibrary extends ToStellarLibrarySpectrumOps
