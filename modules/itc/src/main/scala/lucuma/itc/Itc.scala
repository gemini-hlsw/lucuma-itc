// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import cats.data.NonEmptyList
import eu.timepit.refined.types.numeric.PosLong
import io.circe.*
import io.circe.syntax.*
import lucuma.core.model.NonNegDuration
import lucuma.itc.encoders.given
import lucuma.itc.search._

import scala.concurrent.duration.FiniteDuration
import lucuma.core.math.Wavelength
import lucuma.core.util.Enumerated

final case class UpstreamException(msg: String) extends RuntimeException(msg)

trait Itc[F[_]]:

  /**
   * Compute the exposure time and number required to achieve the desired signal-to-noise under
   * average conditions.
   */
  def calculate(
    targetProfile: TargetProfile,
    observingMode: ObservingMode,
    constraints:   ItcObservingConditions,
    signalToNoise: BigDecimal
  ): F[Itc.CalcResultWithVersion]

  /**
   * Retrieve the graph data for the given mode and exposureTime and exposures
   */
  def calculateGraph(
    targetProfile: TargetProfile,
    observingMode: ObservingMode,
    constraints:   ItcObservingConditions,
    exposureTime:  NonNegDuration,
    exposures:     PosLong
  ): F[Itc.GraphResult]

  /**
   * Calculate the signal to noise from graph data for the given mode and exposureTime and exposures
   */
  def calculateSignalToNoise(
    graph:        Itc.GraphResult,
    atWavelength: Option[Wavelength]
  ): F[Itc.SNCalcResult]

  def itcVersions: F[String]

object Itc:

  def apply[F[_]](using ev: Itc[F]): ev.type = ev

  sealed trait CalcResult extends Product with Serializable {}
  object CalcResult:

    case class Success(
      exposureTime:  FiniteDuration,
      exposures:     Int,
      signalToNoise: BigDecimal
    ) extends CalcResult
        derives Encoder.AsObject

    /** Object is too bright to be observed in the specified mode. */
    case class SourceTooBright(msg: String) extends CalcResult

    /** Generic calculation error */
    case class CalculationError(msg: String) extends CalcResult

    given Encoder[Itc.CalcResult] = Encoder.instance {
      case f: Itc.CalcResult.Success          =>
        Json.obj(("resultType", Json.fromString("Success"))).deepMerge(f.asJson)
      case Itc.CalcResult.CalculationError(m) =>
        Json.obj(("resultType", Json.fromString("Error")), ("msg", Json.fromString(m)))
      case Itc.CalcResult.SourceTooBright(m)  =>
        Json.obj(("resultType", Json.fromString("Error")),
                 ("msg", Json.fromString(s"Source too bright $m"))
        )
    }

  case class CalcResultWithVersion(result: CalcResult, dataVersion: Option[String] = None)

  case class GraphResult(
    dataVersion: String,
    ccds:        NonEmptyList[ItcCcd],
    charts:      NonEmptyList[ItcChartGroup]
  )

  enum ResultType(val tag: String) derives Enumerated:
    case Success          extends ResultType("success")
    case SourceTooBright  extends ResultType("source_too_bright")
    case NoData           extends ResultType("no_data")
    case CalculationError extends ResultType("calculation_error")

  sealed trait SNCalcResult extends Product with Serializable {
    def resultType: ResultType
  }

  object SNCalcResult:
    given Encoder[SNCalcResult] = Encoder.instance { a =>
      Json
        .obj(("resultType", a.resultType.asJson))
        .deepMerge(a match {
          case s: Success => s.asJson
          case _          => null
        })
    }

    case class Success(
      signalToNoise: BigDecimal
    ) extends SNCalcResult
        derives Encoder.AsObject {
      val resultType = ResultType.Success
    }

    /** Object is too bright to be observed in the specified mode. */
    case class SourceTooBright(wellDepth: BigDecimal) extends SNCalcResult {
      val resultType = ResultType.SourceTooBright
    }

    case class NoData() extends SNCalcResult {
      val resultType = ResultType.NoData
    }

    /** Generic calculation error */
    case class CalculationError(msg: String) extends SNCalcResult {
      val resultType = ResultType.CalculationError
    }
