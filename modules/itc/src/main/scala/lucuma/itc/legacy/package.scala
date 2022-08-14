// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.legacy

import lucuma.core.math.Angle
import lucuma.itc.ItcObservingConditions
import lucuma.itc.search.ItcObservationDetails
import lucuma.itc.search.ObservingMode
import lucuma.itc.search.TargetProfile

import scala.concurrent.duration.FiniteDuration

/** Convert model types into OCS2 ITC-compatible types for a spectroscopy request. */
def spectroscopyParams(
  targetProfile:    TargetProfile,
  observingMode:    ObservingMode,
  exposureDuration: FiniteDuration,
  conditions:       ItcObservingConditions,
  exposures:        Int
): ItcParameters =
  ItcParameters(
    source = ItcSourceDefinition.fromTargetProfile(targetProfile),
    observation = ItcObservationDetails(
      calculationMethod = ItcObservationDetails.CalculationMethod.SignalToNoise.Spectroscopy(
        exposures = exposures,
        coadds = None,
        exposureDuration = exposureDuration,
        sourceFraction = 1.0,
        ditherOffset = Angle.Angle0
      ),
      analysisMethod = observingMode.analysisMethod
    ),
    conditions = conditions,
    telescope = ItcTelescopeDetails(
      wfs = ItcWavefrontSensor.OIWFS
    ),
    instrument = ItcInstrumentDetails.fromObservingMode(observingMode)
  )
