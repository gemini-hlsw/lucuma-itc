// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.search.syntax

import lucuma.core.enums.QuasarSpectrum

/**
 * Syntax extensions for missing ocs2Tag items
 */
final class QuasarSpectrumOps(val self: QuasarSpectrum) extends AnyVal {

  def ocs2Tag: String =
    self match {
      case QuasarSpectrum.QS0  => "QSO"
      case QuasarSpectrum.QS02 => "QSO2"
    }

}

trait ToQuasarSpectrumOps {
  implicit def toQuasarSpectrumOps(self: QuasarSpectrum): QuasarSpectrumOps = new QuasarSpectrumOps(
    self
  )
}

object quasar extends ToQuasarSpectrumOps
