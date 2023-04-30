// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client.json

import cats.syntax.either.*
import cats.syntax.option.*
import io.circe.*
import io.circe.generic.semiauto.*
import io.circe.syntax.*
import lucuma.core.math.Wavelength
import lucuma.itc.ItcCcd
import lucuma.itc.client.*

// Decoders for the client don't need to be as generic as the ones for the server.
object decoders {
  given Decoder[Wavelength] = c =>
    c.downField("picometers").as[Int].flatMap { pm =>
      Wavelength.intPicometers
        .getOption(pm)
        .toRight(
          DecodingFailure("Expected positive integer wavelength value for 'picometers'.", c.history)
        )
    }

  given Decoder[ItcCcd]                           = deriveDecoder[ItcCcd]
  given Decoder[OptimizedSeriesResult]            = deriveDecoder[OptimizedSeriesResult]
  given Decoder[OptimizedChartResult]             = deriveDecoder[OptimizedChartResult]
  given Decoder[OptimizedSpectroscopyGraphResult] = deriveDecoder[OptimizedSpectroscopyGraphResult]

}
