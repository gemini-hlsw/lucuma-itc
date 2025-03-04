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

case class CalculationResult(
  versions:    ItcVersions,
  targetTimes: AsterismIntegrationTimeOutcomes
) derives Eq

object CalculationResult:
  given Decoder[CalculationResult] with
    def apply(c: HCursor): Decoder.Result[CalculationResult] =
      for
        v <- c.downField("versions").as[ItcVersions]
        t <- c.downField("targetTimes").as[AsterismIntegrationTimeOutcomes]
      yield CalculationResult(v, t)

  given Eq[CalculationResult] with
    def eqv(x: CalculationResult, y: CalculationResult): Boolean =
      x.versions === y.versions && x.targetTimes === y.targetTimes
