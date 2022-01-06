// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import io.circe.Encoder
import io.circe.generic.semiauto._
import io.circe.Json
import io.circe.syntax._

case class ItcParameters(
  source:      ItcSourceDefinition,
  observation: ItcObservationDetails,
  conditions:  ItcObservingConditions,
  telescope:   ItcTelescopeDetails,
  instrument:  ItcInstrumentDetails
)

object ItcParameters {
  implicit val encoder: Encoder[ItcParameters] =
    deriveEncoder[ItcParameters]
    // new Encoder[ItcParameters] {
    //   def apply(p: ItcParameters): Json =
    //     p.source.asJson.deepMerge(Json.obj("observation" -> p.observation.asJson))
    // }
}
