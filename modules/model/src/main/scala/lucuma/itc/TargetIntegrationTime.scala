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
import lucuma.core.math.Wavelength
import lucuma.itc.encoders.given

case class TargetIntegrationTime(
  times:           Option[Zipper[IntegrationTime]],
  bandOrLine:      Either[Band, Wavelength],
  signalToNoiseAt: Option[SignalToNoiseAt]
):
  def focusIndex(index: Int): Option[TargetIntegrationTime] =
    times.flatMap:
      _.focusIndex(index)
        .map(newTimes => copy(times = Some(newTimes)))

object TargetIntegrationTime:
  given Encoder[TargetIntegrationTime] = t =>
    val common = Json
      .obj(
        "band"            -> t.bandOrLine.left.toOption.asJson,
        "emissionLine"    -> t.bandOrLine.toOption.asJson,
        "signalToNoiseAt" -> t.signalToNoiseAt.asJson
      )
    t.times
      .map(_.asJson.deepMerge(common))
      .getOrElse(
        common.deepMerge(
          Json.obj("all" -> Json.Null, "index" -> Json.Null, "selected" -> Json.Null)
        )
      )

  given Order[TargetIntegrationTime] = Order.by(_.times.map(_.focus))
