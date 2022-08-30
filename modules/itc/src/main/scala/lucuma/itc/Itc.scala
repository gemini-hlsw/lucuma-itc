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
   * Compute the exposure time and number required to achieve the desired signal-to-noise under
   * average conditions.
   */
  def calculateGraph(
    targetProfile: TargetProfile,
    observingMode: ObservingMode,
    constraints:   ItcObservingConditions,
    exposureTime:  NonNegDuration,
    exposures:     PosLong
  ): F[Itc.GraphResult]

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
