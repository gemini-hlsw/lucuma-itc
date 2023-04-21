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
        case "ExposureEstimate" => c.as[Success].widen[ItcResult]
        case "SourceTooBright"  => c.as[SourceTooBright].widen[ItcResult]
        case "CalculationError" => c.as[Error].widen[ItcResult]
        case rt                 =>
          DecodingFailure(s"Couldn't parse ItcResult as success or error: $rt", c.history).asLeft
      }

  given Eq[ItcResult] with
    def eqv(x: ItcResult, y: ItcResult): Boolean =
      (x, y) match {
        case (s0: Success, s1: Success)                 => s0 === s1
        case (e0: SourceTooBright, e1: SourceTooBright) => e0 === e1
        case (e0: Error, e1: Error)                     => e0 === e1
        case _                                          => false
      }

  final case class Error(
    msg: List[String]
  ) extends ItcResult

  object Error {

    given Decoder[Error] with
      def apply(c: HCursor): Decoder.Result[Error] =
        c.downField("msg").as[List[String]].map(Error(_))

    given Eq[Error] =
      Eq.by(_.msg)

  }

  final case class SourceTooBright(
    halfWellTime: BigDecimal
  ) extends ItcResult

  object SourceTooBright {

    given Decoder[SourceTooBright] with
      def apply(c: HCursor): Decoder.Result[SourceTooBright] =
        c.downField("halfWellTime").as[BigDecimal].map(SourceTooBright(_))

    given Eq[SourceTooBright] =
      Eq.by(_.halfWellTime)

  }
  final case class Success(
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

  object Success {

    given Decoder[Success] with
      def apply(c: HCursor): Decoder.Result[Success] =
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
        } yield Success(t, n, s)

    given Eq[Success] =
      Eq.by { a =>
        (
          a.exposureTime.toMicroseconds,
          a.exposures,
          a.signalToNoise
        )
      }

  }

  def error(msg: String): ItcResult =
    Error(List(msg))

  def success(
    exposureTime:  TimeSpan,
    exposures:     NonNegInt,
    signalToNoise: SignalToNoise
  ): ItcResult =
    Success(exposureTime, exposures, signalToNoise)

}
