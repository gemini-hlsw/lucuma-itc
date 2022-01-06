// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.search

import lucuma.core.math.Redshift
import lucuma.core.model.SourceProfile
import lucuma.core.enum.Band
import lucuma.core.math.BrightnessValue
// import lucuma.core.model.BandBrightness
// import lucuma.core.math.BrightnessUnits
// import coulomb.define.UnitDefinition

/** Target properties we need to know at phase zero. */
final case class TargetProfile(
  sourceProfile: SourceProfile,
  band:          Band,
  redshift:      Redshift
) {
  // val integratedBrightness: Option[BandBrightness[BrightnessUnits.Integrated]] =
  //   SourceProfile.integratedBandBrightnessIn(band).headOption(sourceProfile)
  // val surfaceBrightness: Option[BandBrightness[BrightnessUnits.Surface]]       =
  //   SourceProfile.surfaceBandBrightnessIn(band).headOption(sourceProfile)
  // val brightness: Option[BrightnessValue]                                      =
  // integratedBrightness.orElse(surfaceBrightness).map(_.quantity.value)
  // val brightnessUnits: Option[UnitDefinition]                                  =
  //   integratedBrightness.orElse(surfaceBrightness).map(_.quantity.unit.definition)
}
