// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service.syntax

import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthFpu.*

/**
 * Syntax extensions for missing properties. These need to be folded back into the lucuma.core
 * enumerations.
 */
extension (self: GmosNorthFpu)
  def isGNIfu: Boolean =
    self match
      case Ifu2Slits | IfuBlue | IfuRed => true
      case Ns0                          => false
      case Ns1                          => false
      case Ns2                          => false
      case Ns3                          => false
      case Ns4                          => false
      case Ns5                          => false
      case LongSlit_0_25                => false
      case LongSlit_0_50                => false
      case LongSlit_0_75                => false
      case LongSlit_1_00                => false
      case LongSlit_1_50                => false
      case LongSlit_2_00                => false
      case LongSlit_5_00                => false
