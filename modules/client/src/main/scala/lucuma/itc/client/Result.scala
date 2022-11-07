// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client

import cats.Eq
import cats.syntax.eq.*
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.HCursor

final case class Result(
  mode: ObservingModeSpectroscopy,
  itc:  ItcResult
)

object Result {

  given Decoder[Result] with
    def apply(c: HCursor): Decoder.Result[Result] =
      for {
        o <- c.downField("mode").as[ObservingModeSpectroscopy]
        i <- c.downField("itc").as[ItcResult]
      } yield Result(o, i)


  given Eq[Result] with
    def eqv(x: Result, y: Result): Boolean =
      (x.mode === y.mode) &&
        (x.itc === y.itc)

}
