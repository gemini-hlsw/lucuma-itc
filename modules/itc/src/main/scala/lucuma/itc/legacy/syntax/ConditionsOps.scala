// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.legacy.syntax

import lucuma.core.enums.CloudExtinction
import lucuma.core.enums.ImageQuality
import lucuma.core.enums.SkyBackground
import lucuma.core.enums.WaterVapor

trait ConditionsSyntax:
  extension (self: ImageQuality)
    def ocs2Tag: String =
      self match
        case ImageQuality.PointOne     => "PERCENT_20"
        case ImageQuality.PointTwo     => "PERCENT_20"
        case ImageQuality.PointThree   => "PERCENT_20"
        case ImageQuality.PointFour    => "PERCENT_20"
        case ImageQuality.PointSix     => "PERCENT_70"
        case ImageQuality.PointEight   => "PERCENT_70"
        case ImageQuality.OnePointZero => "PERCENT_85"
        case ImageQuality.OnePointFive => "ANY"
        case ImageQuality.TwoPointZero => "ANY"

  extension (self: CloudExtinction)
    def ocs2Tag: String =
      self match
        case CloudExtinction.PointOne                                 => "PERCENT_50"
        case CloudExtinction.PointThree                               => "PERCENT_70"
        case CloudExtinction.PointFive | CloudExtinction.OnePointZero => "PERCENT_80"
        case CloudExtinction.OnePointFive | CloudExtinction.TwoPointZero |
            CloudExtinction.ThreePointZero =>
          "ANY"

  extension (self: WaterVapor)
    def ocs2Tag: String =
      self match
        case WaterVapor.VeryDry => "PERCENT_20"
        case WaterVapor.Dry     => "PERCENT_50"
        case WaterVapor.Median  => "PERCENT_80"
        case WaterVapor.Wet     => "ANY"

  extension (self: SkyBackground)
    def ocs2Tag: String =
      self match
        case SkyBackground.Darkest => "PERCENT_20"
        case SkyBackground.Dark    => "PERCENT_50"
        case SkyBackground.Gray    => "PERCENT_80"
        case SkyBackground.Bright  => "ANY"

object conditions extends ConditionsSyntax
