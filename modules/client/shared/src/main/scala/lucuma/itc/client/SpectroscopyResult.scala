// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client

import cats.Eq
import cats.derived.*
import cats.syntax.eq.*
import io.circe.Decoder
import io.circe.HCursor
import lucuma.itc.AsterismIntegrationTimeOutcomes
import lucuma.itc.Error
import lucuma.itc.ItcVersions
import lucuma.itc.client.json.decoders.given

case class SpectroscopyResult(
  versions:    ItcVersions,
  targetTimes: AsterismIntegrationTimeOutcomes
) derives Eq

object SpectroscopyResult:
  given Decoder[SpectroscopyResult] with
    def apply(c: HCursor): Decoder.Result[SpectroscopyResult] =
      for
        v <- c.downField("versions").as[ItcVersions]
        t <- c.downField("targetTimes").as[AsterismIntegrationTimeOutcomes]
      yield SpectroscopyResult(v, t)

  given Eq[SpectroscopyResult] with
    def eqv(x: SpectroscopyResult, y: SpectroscopyResult): Boolean =
      x.versions === y.versions && x.targetTimes === y.targetTimes
