// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client

import cats.syntax.traverse.*
import clue.GraphQLOperation
import io.circe.Decoder
import io.circe.Encoder
import io.circe.HCursor
import io.circe.Json
import io.circe.JsonObject

object SpectroscopyQuery extends GraphQLOperation[Unit] {

  type Data      = SpectroscopyResult
  type Variables = SpectroscopyModeInput

  override val document: String =
    """
      query Spectroscopy($spec: SpectroscopyModeInput!) {
        spectroscopyExposureTime(input: $spec) {
          serverVersion
          dataVersion
          result {
            __typename
            ... on ExposureEstimate {
              exposures
              exposureTime {
                microseconds
              }
              signalToNoise
            }
            ... on SourceTooBright {
              halfWellTime
            }
            ... on CalculationError {
              originalMessages
            }
          }
        }
      }
    """

  override val varEncoder: Encoder.AsObject[Variables] =
    Encoder.AsObject.instance[SpectroscopyModeInput] { input =>
      JsonObject(
        "spec" -> Encoder[SpectroscopyModeInput].apply(input)
      )
    }

  override val dataDecoder: Decoder[SpectroscopyResult] =
    (c: HCursor) => c.downField("spectroscopyExposureTime").as[SpectroscopyResult]

}
