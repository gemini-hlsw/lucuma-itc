// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

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

sealed trait IntegrationTimeResult extends Product with Serializable

object IntegrationTimeResult:
  given Encoder[IntegrationTimeResult] = Encoder.instance { a =>
    a match {
      case s @ ExposureTimeSuccess(_, _, _) => s.asJson
      case w @ SourceTooBright(_)           => w.asJson
      case w @ CalculationError(_)          => w.asJson
    }
  }

  case class ExposureTimeSuccess(
    exposureTime:  TimeSpan,
    exposures:     PosInt,
    signalToNoise: SignalToNoise
  ) extends IntegrationTimeResult
      derives Encoder.AsObject

  case class SourceTooBright(halfWellTime: BigDecimal) extends IntegrationTimeResult
      derives Encoder.AsObject

  /** Generic calculation error */
  case class CalculationError(msg: List[String]) extends IntegrationTimeResult
      derives Encoder.AsObject

  object CalculationError {
    def apply(msg: String): CalculationError = CalculationError(List(msg))
  }

case class ExposureTimeCalculationResult(
  serverVersion: String,
  dataVersion:   String,
  mode:          ObservingMode.Spectroscopy,
  result:        IntegrationTimeResult
) derives Encoder.AsObject
