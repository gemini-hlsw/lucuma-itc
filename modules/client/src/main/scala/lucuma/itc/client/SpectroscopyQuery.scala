// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client

import cats.syntax.traverse.*
import clue.GraphQLOperation
import io.circe.Decoder
import io.circe.Encoder
import io.circe.HCursor
import io.circe.Json

object SpectroscopyQuery extends GraphQLOperation[Unit] {

  type Data      = List[SpectroscopyResult]
  type Variables = SpectroscopyModeInput

  override val document: String =
    """
      query Spectroscopy($spec: SpectroscopyModeInput!) {
        spectroscopy(input: $spec) {
          serverVersion
          dataVersion
          results {
            itc {
              ... on ItcSuccess {
                exposures
                exposureTime {
                  microseconds
                }
                signalToNoise
                resultType
              }
              ... on ItcError {
                msg
                resultType
              }
            }
          }
        }
      }
    """

  override val varEncoder: Encoder[Variables] =
    Encoder.instance[SpectroscopyModeInput] { input =>
      Json.obj(
        "spec" -> Encoder[SpectroscopyModeInput].apply(input)
      )
    }

  override val dataDecoder: Decoder[List[SpectroscopyResult]] =
    (c: HCursor) => {
      println(c.value.spaces2)
      for {
        lst <- c.downField("spectroscopy").as[List[Json]]
        spc <- lst.traverse(_.hcursor.as[SpectroscopyResult])
      } yield spc
    }

}
