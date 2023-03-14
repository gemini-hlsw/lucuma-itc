// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.search

import cats._
import cats.data.NonEmptyList
import cats.syntax.all._
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.*
import io.circe.syntax.*
import lucuma.core.enums._
import lucuma.itc.ExposureCalculationResult
import lucuma.itc.ItcCcd
import lucuma.itc.ItcChart
import lucuma.itc.ItcObservingConditions
import lucuma.itc.given

sealed trait Result:
  def mode: ObservingMode
  def itc: ExposureCalculationResult

object Result:
  case class Spectroscopy(mode: ObservingMode.Spectroscopy, itc: ExposureCalculationResult)
      derives Encoder.AsObject

case class SpectroscopyResults(
  serverVersion: String,
  dataVersion:   Option[String],
  results:       List[Result.Spectroscopy]
) derives Encoder.AsObject

case class SpectroscopyGraphResults(
  serverVersion: String,
  dataVersion:   Option[String],
  ccds:          NonEmptyList[ItcCcd],
  charts:        NonEmptyList[ItcChart]
) derives Encoder.AsObject

case class ItcVersions(
  serverVersion: String,
  dataVersion:   Option[String]
) derives Encoder.AsObject
