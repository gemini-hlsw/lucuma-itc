// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client

import cats.syntax.all.*
import eu.timepit.refined.types.numeric.*
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.HCursor
import lucuma.core.math.SignalToNoise
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime

given Decoder[IntegrationTime] with
  def apply(c: HCursor): Decoder.Result[IntegrationTime] =
    for {
      t <- c.downField("exposureTime")
             .downField("microseconds")
             .as[Long]
             .flatMap(l =>
               TimeSpan
                 .fromMicroseconds(l)
                 .toRight(
                   DecodingFailure(s"Negative exposure time is not supported: $l", c.history)
                 )
             )
      n <- c.downField("exposures")
             .as[Int]
             .flatMap(n => PosInt.from(n).leftMap(m => DecodingFailure(m, c.history)))
      s <- c.downField("signalToNoise")
             .as[SignalToNoise]
    } yield IntegrationTime(t, n, s)
