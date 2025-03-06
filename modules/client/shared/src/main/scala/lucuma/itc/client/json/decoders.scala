// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client.json

import cats.syntax.either.*
import eu.timepit.refined.types.numeric.NonNegInt
import io.circe.*
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.generic.semiauto.*
import lucuma.core.data.Zipper
import lucuma.core.data.ZipperCodec.given
import lucuma.core.enums.Band
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.util.TimeSpan
import lucuma.itc.AsterismTimesAndGraphsOutcomes
import lucuma.itc.Error
import lucuma.itc.IntegrationTime
import lucuma.itc.ItcCcd
import lucuma.itc.ItcGraph
import lucuma.itc.ItcSeries
import lucuma.itc.TargetIntegrationTime
import lucuma.itc.TargetIntegrationTimeOutcome
import lucuma.itc.client.*
import lucuma.itc.SignalToNoiseAt
import lucuma.itc.SingleSN
import lucuma.itc.FinalSN

// Decoders for the client don't need to be as generic as the ones for the server.
object decoders:
  given Decoder[Wavelength] = c =>
    c.downField("picometers").as[Int].flatMap { pm =>
      Wavelength.intPicometers
        .getOption(pm)
        .toRight(
          DecodingFailure("Expected positive integer wavelength value for 'picometers'.", c.history)
        )
    }

  given Decoder[TimeSpan] = c =>
    c.downField("microseconds")
      .as[Long]
      .flatMap(l =>
        TimeSpan
          .fromMicroseconds(l)
          .toRight(
            DecodingFailure(s"Negative exposure time is not supported: $l", c.history)
          )
      )

  given Decoder[NonNegInt] = c =>
    c.as[Int]
      .flatMap(l => NonNegInt.from(l).leftMap(m => DecodingFailure(m, c.history)))

  given Decoder[IntegrationTime] = c =>
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
      n <- c.downField("exposureCount")
             .as[Int]
             .flatMap(n => NonNegInt.from(n).leftMap(m => DecodingFailure(m, c.history)))
    } yield IntegrationTime(t, n)

  given Decoder[SignalToNoiseAt] = c =>
    for {
      w <- c.downField("wavelength").as[Wavelength]
      s <- c.downField("single").as[SignalToNoise]
      t <- c.downField("total").as[SignalToNoise]
    } yield SignalToNoiseAt(w, SingleSN(s), FinalSN(t))

  given Decoder[TargetIntegrationTime] = c =>
    for
      bandOrLine <-
        c.downField("band")
          .as[Band]
          .map(_.asLeft)
          .orElse(c.downField("emissionLine").as[Wavelength].map(_.asRight))
      times      <- c.as[Option[Zipper[IntegrationTime]]]
      sn         <- c.as[Option[SignalToNoiseAt]]
    yield TargetIntegrationTime(times, bandOrLine, sn)

  given Decoder[ItcCcd]    = deriveDecoder[ItcCcd]
  given Decoder[ItcSeries] = deriveDecoder[ItcSeries]
  given Decoder[ItcGraph]  = deriveDecoder[ItcGraph]

  given Decoder[TargetIntegrationTimeOutcome] =
    Decoder[TargetIntegrationTime]
      .map(_.asRight)
      .or(Decoder[Error].map(_.asLeft))
      .map(TargetIntegrationTimeOutcome(_))

