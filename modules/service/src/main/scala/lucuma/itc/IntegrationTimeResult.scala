// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import cats.data.NonEmptyList
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.*
import io.circe.syntax.*
import lucuma.core.math.SignalToNoise
import lucuma.core.model.NonNegDuration
import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan
import lucuma.itc.encoders.given
import lucuma.itc.search.ObservingMode

import scala.concurrent.duration.FiniteDuration

sealed trait IntegrationTimeError extends RuntimeException {
  def message: String
}

case class SourceTooBright(halfWellTime: BigDecimal) extends IntegrationTimeError
    derives Encoder.AsObject {
  val message: String = f"Source saturates in $halfWellTime%.2f seconds"
}

/** Generic calculation error */
case class CalculationError(msg: List[String]) extends IntegrationTimeError
    derives Encoder.AsObject {
  val message: String = msg.mkString("\n")
}

object CalculationError {
  def apply(msg: String): CalculationError = CalculationError(List(msg))
}

case class IntegrationTimeCalculationResult(
  serverVersion: String,
  dataVersion:   String,
  mode:          ObservingMode,
  result:        IntegrationTime
) derives Encoder.AsObject
