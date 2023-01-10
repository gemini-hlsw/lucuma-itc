// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client.json

import io.circe.Json
import io.circe.syntax.*
import lucuma.core.syntax.enumerated.*
import lucuma.core.syntax.string.*
import lucuma.core.util.Enumerated

trait JsonOps {

  extension [A: Enumerated](a: A)
    def asScreamingJson: Json =
      a.tag.toScreamingSnakeCase.asJson

}

object syntax extends JsonOps
