// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.search.syntax

import lucuma.core.enum.GmosSouthFpu
import lucuma.core.enum.GmosSouthFpu._
import lucuma.core.math.Angle

/**
 * Syntax extensions for missing properties. These need to be folded back into the lucuma.core
 * enumerations.
 */
final class GmosSouthFpuOps(val self: GmosSouthFpu) extends AnyVal {

  def effectiveSlitWidth: Angle =
    self match {
      case Ifu2Slits     => Angle.fromMicroarcseconds(310000)
      case IfuBlue       => Angle.fromMicroarcseconds(310000)
      case IfuRed        => Angle.fromMicroarcseconds(310000)
      case IfuNS2Slits   => Angle.fromMicroarcseconds(310000)
      case IfuNSBlue     => Angle.fromMicroarcseconds(310000)
      case IfuNSRed      => Angle.fromMicroarcseconds(310000)
      case Ns1           => Angle.fromMicroarcseconds(500000)
      case Ns2           => Angle.fromMicroarcseconds(750000)
      case Ns3           => Angle.fromMicroarcseconds(1000000)
      case Ns4           => Angle.fromMicroarcseconds(1500000)
      case Ns5           => Angle.fromMicroarcseconds(2000000)
      case LongSlit_0_25 => Angle.fromMicroarcseconds(250000)
      case LongSlit_0_50 => Angle.fromMicroarcseconds(500000)
      case LongSlit_0_75 => Angle.fromMicroarcseconds(750000)
      case LongSlit_1_00 => Angle.fromMicroarcseconds(1000000)
      case LongSlit_1_50 => Angle.fromMicroarcseconds(1500000)
      case LongSlit_2_00 => Angle.fromMicroarcseconds(2000000)
      case LongSlit_5_00 => Angle.fromMicroarcseconds(5000000)
      case Bhros         => sys.error("Obsolete")
    }

  def isIfu: Boolean =
    self match {
      case Ifu2Slits | IfuBlue | IfuRed | IfuNS2Slits | IfuNSBlue | IfuNSRed => true
      case Ns1                                                               => false
      case Ns2                                                               => false
      case Ns3                                                               => false
      case Ns4                                                               => false
      case Ns5                                                               => false
      case LongSlit_0_25                                                     => false
      case LongSlit_0_50                                                     => false
      case LongSlit_0_75                                                     => false
      case LongSlit_1_00                                                     => false
      case LongSlit_1_50                                                     => false
      case LongSlit_2_00                                                     => false
      case LongSlit_5_00                                                     => false
      case Bhros                                                             => sys.error("obsolete")
    }

  def isNodAndShuffle: Boolean =
    self match {
      case Ifu2Slits | IfuBlue | IfuRed       => false
      case IfuNS2Slits | IfuNSBlue | IfuNSRed => false
      case Ns1                                => true
      case Ns2                                => true
      case Ns3                                => true
      case Ns4                                => true
      case Ns5                                => true
      case LongSlit_0_25                      => false
      case LongSlit_0_50                      => false
      case LongSlit_0_75                      => false
      case LongSlit_1_00                      => false
      case LongSlit_1_50                      => false
      case LongSlit_2_00                      => false
      case LongSlit_5_00                      => false
      case Bhros                              => sys.error("obsolete")
    }

  def ocs2Tag: String =
    self match {
      case Ifu2Slits     => "IFU_1"
      case IfuNS2Slits   => "IFU_N"
      case IfuBlue       => "IFU_2"
      case IfuNSBlue     => "IFU_NS-B"
      case IfuRed        => "IFU_3"
      case IfuNSRed      => "IFU_NS-R"
      case Ns1           => "NS_1"
      case Ns2           => "NS_2"
      case Ns3           => "NS_3"
      case Ns4           => "NS_4"
      case Ns5           => "NS_5"
      case LongSlit_0_25 => "LONGSLIT_1"
      case LongSlit_0_50 => "LONGSLIT_2"
      case LongSlit_0_75 => "LONGSLIT_3"
      case LongSlit_1_00 => "LONGSLIT_4"
      case LongSlit_1_50 => "LONGSLIT_5"
      case LongSlit_2_00 => "LONGSLIT_6"
      case LongSlit_5_00 => "LONGSLIT_7"
      case Bhros         => "BHROs"
    }

}

trait ToGmosSouthFpuOps {
  implicit def toGmosSouthFpuOps(self: GmosSouthFpu): GmosSouthFpuOps =
    new GmosSouthFpuOps(self)
}

object gmossouthfpu extends ToGmosSouthFpuOps
