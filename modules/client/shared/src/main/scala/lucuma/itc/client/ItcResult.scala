// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client

import cats.Eq
import cats.syntax.all.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.numeric.*
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.HCursor
import lucuma.core.math.SignalToNoise
import lucuma.core.syntax.time.*
import lucuma.core.util.TimeSpan

import java.math.MathContext

sealed trait ItcResult

object ItcResult {

  given Decoder[ItcResult] with
    def apply(c: HCursor): Decoder.Result[ItcResult] =
      c.downField("__typename").as[String].flatMap {
        case "IntegrationTime"  => c.as[IntegrationTime].widen[ItcResult]
        case "SourceTooBright"  => c.as[SourceTooBright].widen[ItcResult]
        case "CalculationError" => c.as[CalculationError].widen[ItcResult]
        case rt                 =>
          DecodingFailure(s"Couldn't parse ItcResult as success or error: $rt", c.history).asLeft
      }

  given Eq[ItcResult] with
    def eqv(x: ItcResult, y: ItcResult): Boolean =
      (x, y) match {
        case (s0: IntegrationTime, s1: IntegrationTime)   => s0 === s1
        case (e0: SourceTooBright, e1: SourceTooBright)   => e0 === e1
        case (e0: CalculationError, e1: CalculationError) => e0 === e1
        case _                                            => false
      }

  final case class CalculationError(
    originalMessages: List[String]
  ) extends ItcResult {
    val message: String = originalMessages.mkString(", ")
  }

  object CalculationError {

    given Decoder[CalculationError] with
      def apply(c: HCursor): Decoder.Result[CalculationError] =
        c.downField("originalMessages").as[List[String]].map(CalculationError(_))

    given Eq[CalculationError] =
      Eq.by(_.originalMessages)

  }

  final case class SourceTooBright(
    halfWellTime: BigDecimal
  ) extends ItcResult {
    val message: String = s"Source too bright, it saturates at $halfWellTime"
  }

  object SourceTooBright {

    given Decoder[SourceTooBright] with
      def apply(c: HCursor): Decoder.Result[SourceTooBright] =
        c.downField("halfWellTime")
          .as[BigDecimal]
          .map(SourceTooBright(_))

    given Eq[SourceTooBright] =
      Eq.by(_.halfWellTime)

  }
  final case class IntegrationTime(
    exposureTime:  TimeSpan,
    exposures:     NonNegInt,
    signalToNoise: SignalToNoise
  ) extends ItcResult {

    def stepSignalToNoise: Option[SignalToNoise] =
      SignalToNoise.FromBigDecimalRounding.getOption(
        BigDecimal(
          (signalToNoise.toBigDecimal * signalToNoise.toBigDecimal / exposures.value)
            .underlying()
            .sqrt(MathContext.DECIMAL128)
        )
      )
  }

  object IntegrationTime {

    given Decoder[IntegrationTime] with
      def apply(c: HCursor): Decoder.Result[IntegrationTime] =
        for {
          t <- c.downField("exposureTime")
                 .downField("microseconds")
                 .as[Long]
                 .flatMap(l =>
                   TimeSpan
                     .fromMicroseconds(l)
                     .toRight(
                       DecodingFailure(s"Negative exposure time is not supported: $l", c.history)
                     )
                 )
          n <- c.downField("exposures")
                 .as[Int]
                 .flatMap(n => NonNegInt.from(n).leftMap(m => DecodingFailure(m, c.history)))
          s <- c.downField("signalToNoise")
                 .as[SignalToNoise]
        } yield IntegrationTime(t, n, s)

    given Eq[IntegrationTime] =
      Eq.by { a =>
        (
          a.exposureTime.toMicroseconds,
          a.exposures,
          a.signalToNoise
        )
      }

  }

  def error(msg: String): ItcResult =
    CalculationError(List(msg))

  def success(
    exposureTime:  TimeSpan,
    exposures:     NonNegInt,
    signalToNoise: SignalToNoise
  ): ItcResult =
    IntegrationTime(exposureTime, exposures, signalToNoise)

}
