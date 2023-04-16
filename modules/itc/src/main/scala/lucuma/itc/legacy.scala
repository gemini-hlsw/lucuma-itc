// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import cats.data.NonEmptyList
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.numeric.PosLong
import io.circe.*
import io.circe.syntax.*
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.model.NonNegDuration
import lucuma.core.util.Enumerated
import lucuma.itc.encoders.given
import lucuma.itc.legacy.ItcRemoteCcd
import lucuma.itc.search.*

import scala.concurrent.duration.FiniteDuration

case class LegacyItcResult(mode: ObservingMode, itc: LegacyExposureCalculationResult)

object LegacyItcResult:
  case class Spectroscopy(mode: ObservingMode.Spectroscopy, itc: LegacyExposureCalculationResult)
      derives Encoder.AsObject

  case class SpectroscopyExposureTime(
    mode:   ObservingMode.Spectroscopy,
    result: ExposureTimeResult
  ) derives Encoder.AsObject

// Contains the result of exposure calculations either success or error
sealed trait LegacyExposureCalculationResult extends Product with Serializable {}
object LegacyExposureCalculationResult:

  case class Success(
    exposureTime:  FiniteDuration,
    exposures:     PosInt,
    signalToNoise: SignalToNoise
  ) extends LegacyExposureCalculationResult
      derives Encoder.AsObject

  /** Object is too bright to be observed in the specified mode. */
  case class SourceTooBright(msg: String) extends LegacyExposureCalculationResult

  /** Generic calculation error */
  case class CalculationError(msg: String) extends LegacyExposureCalculationResult

  given Encoder[LegacyExposureCalculationResult] = Encoder.instance {
    case f: LegacyExposureCalculationResult.Success          =>
      Json.obj(("resultType", Json.fromString("Success"))).deepMerge(f.asJson)
    case LegacyExposureCalculationResult.CalculationError(m) =>
      Json.obj(("resultType", Json.fromString("Error")), ("msg", Json.fromString(m)))
    case LegacyExposureCalculationResult.SourceTooBright(m)  =>
      Json.obj(("resultType", Json.fromString("Error")),
               ("msg", Json.fromString(s"Source too bright $m"))
      )
  }

case class LegacyResult(mode: ObservingMode.Spectroscopy, itc: LegacyExposureCalculationResult)
    derives Encoder.AsObject

case class LegacySpectroscopyResult(
  serverVersion: String,
  dataVersion:   Option[String] = None,
  results:       List[LegacyResult]
) derives Encoder.AsObject
