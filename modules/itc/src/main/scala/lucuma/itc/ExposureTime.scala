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

enum ExposureTimeResultType(val tag: String) derives Enumerated:
  case Success          extends ExposureTimeResultType("success")
  case SourceTooBright  extends ExposureTimeResultType("source_too_bright")
  case CalculationError extends ExposureTimeResultType("calculation_error")

sealed trait ExposureTimeResult extends Product with Serializable {
  val resultType: ExposureTimeResultType
  def toLegacy: LegacyExposureCalculationResult
}

object ExposureTimeResult:
  given Encoder[ExposureTimeResult] = Encoder.instance { a =>
    Json
      .obj(("resultType", a.resultType.asJson))
      .deepMerge(a match {
        case s @ ExposureTimeSuccess(_, _, _) => s.asJson
        case w @ SourceTooBright(_)           => w.asJson
        case w @ CalculationError(_)          => w.asJson
      })
  }

  case class ExposureTimeSuccess(
    exposureTime:  FiniteDuration,
    exposures:     PosInt,
    signalToNoise: SignalToNoise
  ) extends ExposureTimeResult
      derives Encoder.AsObject {
    val resultType                                = ExposureTimeResultType.Success
    def toLegacy: LegacyExposureCalculationResult =
      LegacyExposureCalculationResult.Success(exposureTime, exposures, signalToNoise)
  }

  case class SourceTooBright(halfWellTime: BigDecimal) extends ExposureTimeResult
      derives Encoder.AsObject {
    val resultType                                = ExposureTimeResultType.SourceTooBright
    def toLegacy: LegacyExposureCalculationResult = LegacyExposureCalculationResult.SourceTooBright(
      s"Target is too bright. Well half filled in $halfWellTime"
    )
  }

  /** Generic calculation error */
  case class CalculationError(msg: String) extends ExposureTimeResult derives Encoder.AsObject {
    val resultType                                = ExposureTimeResultType.CalculationError
    def toLegacy: LegacyExposureCalculationResult =
      LegacyExposureCalculationResult.CalculationError(msg)
  }

case class ExposureTimeModeResult(
  mode:   ObservingMode.Spectroscopy,
  result: ExposureTimeResult
) derives Encoder.AsObject {
  def toLegacy: List[LegacyResult] =
    List(LegacyResult(mode, result.toLegacy))
}

case class ExposureTimeCalculationResult(
  serverVersion: String,
  dataVersion:   String,
  results:       List[ExposureTimeModeResult]
) derives Encoder.AsObject {
  def toLegacy: List[LegacySpectroscopyResult] =
    results.map(x =>
      LegacySpectroscopyResult(
        serverVersion = serverVersion,
        dataVersion = dataVersion.some,
        x.toLegacy
      )
    )
}
