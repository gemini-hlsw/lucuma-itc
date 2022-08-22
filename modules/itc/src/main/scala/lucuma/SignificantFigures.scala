// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import eu.timepit.refined.types.numeric.PosInt
import io.circe._
import io.circe.refined._

import scala.math._

case class SignificantFigures(xAxis: Option[PosInt], yAxis: Option[PosInt])
    derives Encoder.AsObject,
      Decoder
