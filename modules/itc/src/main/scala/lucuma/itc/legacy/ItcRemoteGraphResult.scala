// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.legacy

import cats.data.NonEmptyList
import io.circe.Decoder
import lucuma.itc.ItcChartGroup

case class ItcRemoteGraphResult(
  versionToken: String,
  ccds:         NonEmptyList[ItcRemoteCcd],
  groups:       NonEmptyList[ItcChartGroup]
)

case class ItcRemoteVersion(
  versionToken: String
) derives Decoder
