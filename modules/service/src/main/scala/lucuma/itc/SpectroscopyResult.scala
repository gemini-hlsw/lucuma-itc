// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.itc.search.ObservingMode

case class SpectroscopyResult(
  versions:    ItcVersions,
  mode:        ObservingMode,
  targetTimes: AsterismIntegrationTimeOutcomes
)

object SpectroscopyResult:
  given Encoder[SpectroscopyResult] = r =>
    Json
      .obj(
        "versions"       -> r.versions.asJson,
        "mode"           -> r.mode.asJson,
        "targetTimes"    -> r.targetTimes.asJson,
        "brightestIndex" -> r.targetTimes.brightestIndex.asJson,
        "brightest"      -> r.targetTimes.brightest.asJson
      )
