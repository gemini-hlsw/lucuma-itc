// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client

import cats.Eq
import cats.syntax.all.*
import io.circe.Decoder
import io.circe.HCursor
import lucuma.core.data.Zipper
import lucuma.core.data.ZipperCodec.given
import lucuma.itc.IntegrationTime

final case class IntegrationTimeResult(
  versions: ItcVersions,
  result:   Zipper[IntegrationTime]
)

object IntegrationTimeResult {

  given Decoder[IntegrationTimeResult] with
    def apply(c: HCursor): Decoder.Result[IntegrationTimeResult] =
      for {
        v <- c.as[ItcVersions]
        z <- c.as[Zipper[IntegrationTime]]
      } yield IntegrationTimeResult(v, z)

  given Eq[IntegrationTimeResult] with
    def eqv(x: IntegrationTimeResult, y: IntegrationTimeResult): Boolean =
      x.versions === y.versions && x.result === y.result

}
