// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import io.circe.*
import io.circe.syntax.*
import lucuma.core.util.Enumerated

enum ErrorCode(val tag: String) derives Enumerated:
  case SourceTooBright extends ErrorCode("source_too_bright")

sealed trait Extension(val errorCode: ErrorCode)

object Extension:
  given Encoder.AsObject[Extension] = r =>
    JsonObject(
      "errorCode" -> r.errorCode.asJson
    )

case class SourceTooBrightExtension(hw: BigDecimal) extends Extension(ErrorCode.SourceTooBright)

object SourceTooBrightExtension:
  given Encoder.AsObject[SourceTooBrightExtension] = r =>
    JsonObject(
      "errorCode" -> r.errorCode.asJson,
      "error"     -> Json.obj("halfWell" -> r.hw.asJson)
    )

  given Decoder[SourceTooBrightExtension] = c =>
    for {
      _  <- c.downField("errorCode").as[ErrorCode].flatMap { case ErrorCode.SourceTooBright =>
              Right(ErrorCode.SourceTooBright)
            // When we add more Extensions we'd need to handdle that case as:
            // case _ => Left(DecodingFailure("SourceTooBrightExtension", c.history))
            }
      hw <- c.downField("error").downField("halfWell").as[BigDecimal]
    } yield SourceTooBrightExtension(hw)
