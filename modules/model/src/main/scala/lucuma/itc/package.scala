// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import cats.data.NonEmptyList
import io.circe.*
import lucuma.itc.encoders.given

case class ItcVersions(
  serverVersion: String,
  dataVersion:   Option[String]
) derives Encoder.AsObject
