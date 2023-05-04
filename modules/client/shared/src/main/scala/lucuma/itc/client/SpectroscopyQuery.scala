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
import lucuma.itc.client.OptimizedSpectroscopyGraphResult
import lucuma.itc.client.json.decoders.given

object SpectroscopyQuery extends GraphQLOperation[Unit] {

  type Data      = SpectroscopyResult
  type Variables = SpectroscopyIntegrationTimeInput

  override val document: String =
    """
      query Spectroscopy($spec: SpectroscopyIntegrationTimeInput!) {
        spectroscopyIntegrationTime(input: $spec) {
          serverVersion
          dataVersion
          results {
            exposures
            exposureTime {
              microseconds
            }
            signalToNoise
          }
        }
      }
    """

  override val varEncoder: Encoder.AsObject[Variables] =
    Encoder.AsObject.instance[SpectroscopyIntegrationTimeInput] { input =>
      JsonObject(
        "spec" -> Encoder[SpectroscopyIntegrationTimeInput].apply(input)
      )
    }

  override val dataDecoder: Decoder[SpectroscopyResult] =
    (c: HCursor) => c.downField("spectroscopyIntegrationTime").as[SpectroscopyResult]

}

object SpectroscopyGraphQuery
    extends GraphQLOperation.Typed[Unit,
                                   OptimizedSpectroscopyGraphInput,
                                   OptimizedSpectroscopyGraphResult
    ] {

  val document =
    """
    query($input: OptimizedSpectroscopyGraphInput!) {
      optimizedSpectroscopyGraph(input: $input) {
        serverVersion
        dataVersion
        ccds {
          singleSNRatio
          totalSNRatio
          peakPixelFlux
          ampGain
          maxTotalSNRatio
          maxSingleSNRatio
          wavelengthForMaxTotalSNRatio {
            picometers
          }
          wavelengthForMaxSingleSNRatio {
            picometers
          }
          wellDepth
          warnings {
            msg
          }
        }
        charts {
          chartType
          series {
            title
            seriesType
            dataY
            xAxis {
              start
              end
              count
              min
              max
            }
            yAxis {
              start
              end
              count
              min
              max
            }
          }
        }
      }
    }
  """

  override val varEncoder: Encoder.AsObject[Variables] =
    Encoder.AsObject.instance[OptimizedSpectroscopyGraphInput] { input =>
      JsonObject(
        "input" -> Encoder[OptimizedSpectroscopyGraphInput].apply(input)
      )
    }

  override val dataDecoder: Decoder[OptimizedSpectroscopyGraphResult] =
    (c: HCursor) => c.downField("optimizedSpectroscopyGraph").as[OptimizedSpectroscopyGraphResult]
}
