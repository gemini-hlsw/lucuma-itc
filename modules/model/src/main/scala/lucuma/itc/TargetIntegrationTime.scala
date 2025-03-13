// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import cats.Order
import lucuma.core.data.Zipper
import lucuma.core.data.ZipperCodec.given
import lucuma.core.enums.Band
import lucuma.core.math.Wavelength
import lucuma.core.util.TimeSpan
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*

case class TargetIntegrationTime(
  times:           Zipper[IntegrationTime],
  bandOrLine:      Either[Band, Wavelength],
  signalToNoiseAt: Option[SignalToNoiseAt]
):
  def focusIndex(index: Int): Option[TargetIntegrationTime] =
    times.focusIndex(index).map(newTimes => copy(times = newTimes))

object TargetIntegrationTime:
  given (using
    Encoder[Wavelength],
    Encoder[TimeSpan]
  ): Encoder[TargetIntegrationTime] = t =>
    val common = Json
      .obj(
        "band"            -> t.bandOrLine.left.toOption.asJson,
        "emissionLine"    -> t.bandOrLine.toOption.asJson,
        "signalToNoiseAt" -> t.signalToNoiseAt.asJson
      )
    // Merge common fields with times fields which are now guaranteed to be present
    t.times.asJson.deepMerge(common)

  given Order[TargetIntegrationTime] = Order.by(_.times.focus)
