// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.legacy.syntax

import lucuma.core.enums.SkyBackground
import lucuma.core.enums.WaterVapor
import lucuma.core.model.CloudExtinction
import lucuma.core.model.ImageQuality

trait ConditionsSyntax:
  extension (self: ImageQuality.Point)
    def ocs2Tag: String =
      self match
        case ImageQuality.Point.PointOne     => "PERCENT_20"
        case ImageQuality.Point.PointTwo     => "PERCENT_20"
        case ImageQuality.Point.PointThree   => "PERCENT_20"
        case ImageQuality.Point.PointFour    => "PERCENT_20"
        case ImageQuality.Point.PointSix     => "PERCENT_70"
        case ImageQuality.Point.PointEight   => "PERCENT_70"
        case ImageQuality.Point.OnePointZero => "PERCENT_85"
        case ImageQuality.Point.OnePointFive => "ANY"
        case ImageQuality.Point.TwoPointZero => "ANY"

  extension (self: CloudExtinction.Point)
    def ocs2Tag: String =
      self match
        case CloudExtinction.Point.PointOne                                       => "PERCENT_50"
        case CloudExtinction.Point.PointThree                                     => "PERCENT_70"
        case CloudExtinction.Point.PointFive | CloudExtinction.Point.OnePointZero => "PERCENT_80"
        case CloudExtinction.Point.OnePointFive | CloudExtinction.Point.TwoPointZero |
            CloudExtinction.Point.ThreePointZero =>
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
