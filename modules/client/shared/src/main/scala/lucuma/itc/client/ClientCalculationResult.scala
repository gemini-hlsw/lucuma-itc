// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client

import cats.Eq
import cats.data.NonEmptyList
import cats.derived.*
import cats.syntax.eq.*
import io.circe.Decoder
import io.circe.HCursor
import lucuma.itc.AsterismIntegrationTimeOutcomes
import lucuma.itc.Error
import lucuma.itc.ItcVersions
import lucuma.itc.TargetIntegrationTimeOutcome
import lucuma.itc.client.json.decoders.given

case class ClientCalculationResult(
  targetTimes: AsterismIntegrationTimeOutcomes
) derives Eq

object ClientCalculationResult:
  given Decoder[ClientCalculationResult] with
    def apply(c: HCursor): Decoder.Result[ClientCalculationResult] =
      for t <- c.downField("targetTimes").as[AsterismIntegrationTimeOutcomes]
      yield ClientCalculationResult(t)

  given Eq[ClientCalculationResult] with
    def eqv(x: ClientCalculationResult, y: ClientCalculationResult): Boolean =
      x.targetTimes === y.targetTimes

case class ClientModesResult(
  versions: ItcVersions,
  all:      NonEmptyList[ClientCalculationResult]
) derives Eq

object ClientModesResult:
  given Decoder[ClientModesResult] with
    def apply(c: HCursor): Decoder.Result[ClientModesResult] =
      for
        v   <- c.downField("versions").as[ItcVersions]
        all <- c.downField("all").as[NonEmptyList[ClientCalculationResult]]
      yield ClientModesResult(v, all)
