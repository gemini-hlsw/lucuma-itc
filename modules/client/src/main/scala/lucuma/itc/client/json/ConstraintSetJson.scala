// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client.json

import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ElevationRange
import syntax.*

given Encoder[ConstraintSet] with
  def apply(a: ConstraintSet): Json =
    Json.obj(
      "imageQuality"    -> a.imageQuality.asScreamingJson,
      "cloudExtinction" -> a.cloudExtinction.asScreamingJson,
      "skyBackground"   -> a.skyBackground.asScreamingJson,
      "waterVapor"      -> a.waterVapor.asScreamingJson,
      "elevationRange"  -> (a.elevationRange match {
        case ElevationRange.AirMass(min, max)             =>
          Json.obj(
            "airMass"->
              Json.obj(
                "min" -> min.value.asJson,
                "max" -> max.value.asJson
              )
          )
        case ElevationRange.HourAngle(minHours, maxHours) =>
          Json.obj(
            "hourAngle" ->
              Json.obj(
                "minHours" -> minHours.value.asJson,
                "maxHours" -> maxHours.value.asJson
              )
          )
      })
    )
