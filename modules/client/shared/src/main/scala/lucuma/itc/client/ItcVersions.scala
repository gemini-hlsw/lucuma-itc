// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client

import cats.Eq
import cats.syntax.eq.*
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.HCursor

// ITC versions reported by the server.  You cannot infer ordering from the
// information but they can be used for equality comparisons.
final case class ItcVersions(
  server: String,
  data:   Option[String]
)

object ItcVersions {

  given Decoder[ItcVersions] with
    def apply(c: HCursor): Decoder.Result[ItcVersions] =
      for {
        s <- c.downField("serverVersion").as[String]
        d <- c.downField("dataVersion").as[Option[String]]
      } yield ItcVersions(s, d)

  given Eq[ItcVersions] with
    def eqv(x: ItcVersions, y: ItcVersions): Boolean =
      (x.server === y.server) && (x.data === y.data)

}
