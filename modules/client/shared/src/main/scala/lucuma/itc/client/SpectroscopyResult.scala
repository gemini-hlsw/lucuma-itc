// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client

import cats.Eq
import cats.data.NonEmptyList
import cats.syntax.eq.*
import cats.syntax.traverse.*
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.HCursor
import lucuma.itc.IntegrationTime

final case class SpectroscopyResult(
  versions: ItcVersions,
  result:   NonEmptyList[IntegrationTime]
)

object SpectroscopyResult {

  given Decoder[SpectroscopyResult] with
    def apply(c: HCursor): Decoder.Result[SpectroscopyResult] =
      for {
        v <- c.as[ItcVersions]
        r <- c.downField("results").as[NonEmptyList[IntegrationTime]]
      } yield SpectroscopyResult(v, r)

  given Eq[SpectroscopyResult] with
    def eqv(x: SpectroscopyResult, y: SpectroscopyResult): Boolean =
      x.versions === y.versions && x.result === y.result

}
