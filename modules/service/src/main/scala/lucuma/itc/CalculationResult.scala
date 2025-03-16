// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.model.ExposureTimeMode
import lucuma.core.util.TimeSpan
import lucuma.core.math.Wavelength
import lucuma.itc.encoders.given
import lucuma.itc.service.ObservingMode

case class CalculationResult(
  versions:         ItcVersions,
  mode:             ObservingMode,
  targetTimes:      AsterismIntegrationTimeOutcomes,
  exposureTimeMode: ExposureTimeMode
)

object CalculationResult:
  given (using Encoder[Wavelength], Encoder[TimeSpan]): Encoder[CalculationResult] = r =>
    Json
      .obj(
        "versions"         -> r.versions.asJson,
        "mode"             -> r.mode.asJson,
        "exposureTimeMode" -> r.exposureTimeMode.asJson,
        "targetTimes"      -> r.targetTimes.asJson,
        "brightestIndex"   -> r.targetTimes.brightestIndex.asJson,
        "brightest"        -> r.targetTimes.brightest.asJson
      )
