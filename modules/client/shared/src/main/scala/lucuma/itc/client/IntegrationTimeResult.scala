// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import io.circe.Decoder
import io.circe.HCursor
import lucuma.itc.AsterismIntegrationTimeOutcomes
import lucuma.itc.Error
import lucuma.itc.ItcVersions
import lucuma.itc.client.json.decoders.given

case class IntegrationTimeResult(
  versions:    ItcVersions,
  targetTimes: AsterismIntegrationTimeOutcomes
) derives Eq

object IntegrationTimeResult:
  given Decoder[IntegrationTimeResult] with
    def apply(c: HCursor): Decoder.Result[IntegrationTimeResult] =
      for {
        v <- c.downField("versions").as[ItcVersions]
        t <- c.downField("targetTimes").as[AsterismIntegrationTimeOutcomes]
      } yield IntegrationTimeResult(v, t)

  given Eq[IntegrationTimeResult] with
    def eqv(x: IntegrationTimeResult, y: IntegrationTimeResult): Boolean =
      x.versions === y.versions && x.targetTimes === y.targetTimes
