// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import cats.Order
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.data.Zipper
import lucuma.core.data.ZipperCodec.given
import lucuma.core.enums.Band

case class TargetIntegrationTime(times: Zipper[IntegrationTime], band: Band):
  def focusIndex(index: Int): Option[TargetIntegrationTime] =
    times
      .focusIndex(index)
      .map: newTimes =>
        copy(times = newTimes)

object TargetIntegrationTime:
  given Encoder[TargetIntegrationTime] = t =>
    Json
      .obj("band" -> t.band.asJson)
      .deepMerge(t.times.asJson)

  given Order[TargetIntegrationTime] = Order.by(_.times.focus)
