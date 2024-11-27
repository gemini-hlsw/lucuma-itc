// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import cats.syntax.all.*
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.itc.search.ObservingMode

sealed trait IntegrationTimeError extends RuntimeException {
  def message: String

  override def getMessage(): String = message
}

object IntegrationTimeError {
  def unapply(e: IntegrationTimeError): Option[String] = Some(e.message)
}

case class SourceTooBright(wellHalfFilledSeconds: BigDecimal) extends IntegrationTimeError
    derives Encoder.AsObject {
  val message: String = f"Source too bright, well half filled in $wellHalfFilledSeconds%.2f seconds"
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
  versions:    ItcVersions,
  mode:        ObservingMode,
  targetTimes: AsterismIntegrationTimeOutcomes
)

object IntegrationTimeCalculationResult:
  given Encoder[IntegrationTimeCalculationResult] = r =>
    Json
      .obj(
        "versions"       -> r.versions.asJson,
        "mode"           -> r.mode.asJson,
        "targetTimes"    -> r.targetTimes.asJson,
        "brightestIndex" -> r.targetTimes.brightestIndex.asJson,
        "brightest"      -> r.targetTimes.brightest.asJson
      )
