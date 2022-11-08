// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client

import cats.syntax.traverse.*
import clue.GraphQLOperation
import io.circe.Decoder
import io.circe.Encoder
import io.circe.HCursor
import io.circe.Json

object VersionsQuery extends GraphQLOperation[Unit] {

  type Data      = ItcVersions
  type Variables = Unit

  override val document: String =
    """
      query Versions {
        versions {
          serverVersion
          dataVersion
        }
      }
    """

  override val varEncoder: Encoder[Unit] =
    Encoder[Unit]

  override val dataDecoder: Decoder[ItcVersions] =
    (c: HCursor) => c.downField("versions").as[ItcVersions]

}
