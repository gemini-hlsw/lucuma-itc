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
import lucuma.itc.encoders.given
import lucuma.itc.search.ObservingMode

import scala.concurrent.duration.FiniteDuration

sealed trait ExposureTimeResult extends Product with Serializable

object ExposureTimeResult:
  given Encoder[ExposureTimeResult] = Encoder.instance { a =>
    a match {
      case s @ ExposureTimeSuccess(_, _, _) => s.asJson
      case w @ SourceTooBright(_)           => w.asJson
      case w @ CalculationError(_)          => w.asJson
    }
  }

  case class ExposureTimeSuccess(
    exposureTime:  FiniteDuration,
    exposures:     PosInt,
    signalToNoise: SignalToNoise
  ) extends ExposureTimeResult
      derives Encoder.AsObject

  case class SourceTooBright(halfWellTime: BigDecimal) extends ExposureTimeResult
      derives Encoder.AsObject

  /** Generic calculation error */
  case class CalculationError(msg: List[String]) extends ExposureTimeResult derives Encoder.AsObject

  object CalculationError {
    def apply(msg: String): CalculationError = CalculationError(List(msg))
  }

case class ExposureTimeCalculationResult(
  serverVersion: String,
  dataVersion:   String,
  mode:          ObservingMode.Spectroscopy,
  result:        ExposureTimeResult
) derives Encoder.AsObject
