// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
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
      "imageQuality"    -> Json.obj(
        "arcsec" -> a.imageQuality.toImageQuality.toArcSeconds.asJson
      ),
      "cloudExtinction" -> Json.obj(
        "extinction" -> a.cloudExtinction.toCloudExtinction.toVegaMagnitude.asJson
      ),
      "skyBackground"   -> a.skyBackground.asScreamingJson,
      "waterVapor"      -> a.waterVapor.asScreamingJson,
      "elevationRange"  -> (a.elevationRange match {
        case ElevationRange.ByAirMass(min, max)             =>
          Json.obj(
            "airMass" ->
              Json.obj(
                "min" -> min.toBigDecimal.asJson,
                "max" -> max.toBigDecimal.asJson
              )
          )
        case ElevationRange.ByHourAngle(minHours, maxHours) =>
          Json.obj(
            "hourAngle" ->
              Json.obj(
                "minHours" -> minHours.toBigDecimal.asJson,
                "maxHours" -> maxHours.toBigDecimal.asJson
              )
          )
      })
    )
