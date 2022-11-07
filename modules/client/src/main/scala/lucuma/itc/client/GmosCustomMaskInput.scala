// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client

import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.GmosCustomSlitWidth
import lucuma.itc.client.json.syntax.*

final case class GmosCustomMaskInput(
  slitWidth: GmosCustomSlitWidth,
  fileName:  String
)

object GmosCustomMaskInput {

  given Encoder[GmosCustomMaskInput] with
    def apply(a: GmosCustomMaskInput): Json =
      Json.obj(
        "slitWidth" -> a.slitWidth.asScreamingJson,
        "filename"  -> a.fileName.asJson  // NOTE: all lower case tag "filename"
      )

}
