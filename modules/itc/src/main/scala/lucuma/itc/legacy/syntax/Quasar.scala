// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.legacy.syntax

import lucuma.core.enums.QuasarSpectrum

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
