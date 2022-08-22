// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.legacy

import cats.data.NonEmptyList
import lucuma.itc.ItcChart
import lucuma.itc.ItcCcd

case class ItcRemoteGraphResult(versionToken: String, ccds: NonEmptyList[ItcCcd], charts: NonEmptyList[ItcChart])
