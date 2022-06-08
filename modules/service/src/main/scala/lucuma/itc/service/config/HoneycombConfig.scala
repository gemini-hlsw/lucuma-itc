// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service.config

import cats.implicits._
import ciris.ConfigValue.configValueNonEmptyParallel
import ciris._

case class HoneycombConfig(
  writeKey: String,
  dataset:  String
)

object HoneycombConfig {

  val config: ConfigValue[Effect, HoneycombConfig] =
    (envOrProp("HONEYCOMB_WRITE_KEY"), envOrProp("HONEYCOMB_DATASET")).mapN((a, b) =>
      HoneycombConfig(a, b)
    )

}
