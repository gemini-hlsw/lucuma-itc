// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client

import cats.Eq
import cats.data.NonEmptyList
import cats.syntax.all.*
import io.circe.Decoder
import io.circe.HCursor
import lucuma.itc.IntegrationTime
import eu.timepit.refined.types.numeric.NonNegInt
import io.circe.DecodingFailure

final case class IntegrationTimeResult(
  versions:       ItcVersions,
  result:         NonEmptyList[IntegrationTime],
  preferredIndex: NonNegInt
)

object IntegrationTimeResult {

  given Decoder[IntegrationTimeResult] with
    def apply(c: HCursor): Decoder.Result[IntegrationTimeResult] =
      for {
        v <- c.as[ItcVersions]
        r <- c.downField("results").as[NonEmptyList[IntegrationTime]]
        i <- c.downField("preferredIndex")
               .as[Int]
               .flatMap(NonNegInt.from(_).leftMap(DecodingFailure(_, Nil)))
      } yield IntegrationTimeResult(v, r, i)

  given Eq[IntegrationTimeResult] with
    def eqv(x: IntegrationTimeResult, y: IntegrationTimeResult): Boolean =
      x.versions === y.versions && x.result === y.result

}
