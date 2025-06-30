// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import cats.data.NonEmptyList
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.core.util.TimeSpan
import lucuma.itc.AsterismIntegrationTimeOutcomes
import lucuma.itc.ItcVersions
import lucuma.itc.encoders.given

case class CalculationResult(
  mode:             ObservingMode,
  targetTimes:      AsterismIntegrationTimeOutcomes,
  exposureTimeMode: ExposureTimeMode
)

case class AllResults(
  versions: ItcVersions,
  all:      NonEmptyList[CalculationResult]
)

object CalculationResult:
  given (using Encoder[Wavelength], Encoder[TimeSpan]): Encoder[CalculationResult] = r =>
    Json
      .obj(
        "mode"             -> r.mode.asJson,
        "exposureTimeMode" -> r.exposureTimeMode.asJson,
        "targetTimes"      -> r.targetTimes.asJson,
        "brightestIndex"   -> r.targetTimes.brightestIndex.asJson,
        "brightest"        -> r.targetTimes.brightest.asJson
      )

object AllResults:
  given (using Encoder[Wavelength], Encoder[TimeSpan]): Encoder[AllResults] = r =>
    Json.obj(
      "versions" -> r.versions.asJson,
      "all"      -> r.all.asJson
    )
