// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import cats.data.NonEmptyList
import io.circe.*

case class SpectroscopyResults(
  serverVersion: String,
  dataVersion:   Option[String],
  results:       List[LegacyResult.Spectroscopy]
) derives Encoder.AsObject

case class SpectroscopyGraphResults(
  serverVersion: String,
  dataVersion:   Option[String],
  ccds:          NonEmptyList[ItcCcd],
  charts:        NonEmptyList[ItcChart]
) derives Encoder.AsObject

case class ItcVersions(
  serverVersion: String,
  dataVersion:   Option[String]
) derives Encoder.AsObject

case class ExposureTimeResults(
  serverVersion: String,
  dataVersion:   Option[String],
  results:       List[LegacyResult.SpectroscopyExposureTime]
) derives Encoder.AsObject
