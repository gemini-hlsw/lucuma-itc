// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import io.circe.Encoder

// N.B. this isn't in the enum package because it's not clear that it will exist in the schema
enum ItcWavefrontSensor(val ocs2Tag: String):
  case PWFS  extends ItcWavefrontSensor("PWFS")
  case OIWFS extends ItcWavefrontSensor("OIWFS")

object ItcWavefrontSensor:
  val encoder: Encoder[ItcWavefrontSensor] = Encoder[String].contramap(_.ocs2Tag)
