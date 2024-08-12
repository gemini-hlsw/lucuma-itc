// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import cats.Eq
import cats.syntax.all.*
import io.circe.*
import io.circe.syntax.*
import lucuma.core.util.Enumerated

enum ErrorCode(val tag: String) derives Enumerated:
  case SourceTooBright extends ErrorCode("source_too_bright")
  case General         extends ErrorCode("general")

enum Error(val code: ErrorCode, val message: String):
  case SourceTooBright(wellHalfFilledSeconds: BigDecimal)
      extends Error(
        ErrorCode.SourceTooBright,
        f"Source too bright, well half filled in $wellHalfFilledSeconds%.2f seconds"
      )
  case General(override val message: String) extends Error(ErrorCode.General, message)

object Error:
  given Eq[Error] = Eq.instance:
    case (SourceTooBright(hw1), SourceTooBright(hw2)) => hw1 === hw2
    case (General(m1), General(m2))                   => m1 === m2
    case _                                            => false

  given Encoder[Error] = e =>
    Json
      .obj(
        "errorCode" -> e.code.asJson,
        "message"   -> e.message.asJson
      )
      .deepMerge(e match
        case SourceTooBright(hw) => Json.obj("wellHalfFilledSeconds" -> hw.asJson)
        case _                   => Json.obj("wellHalfFilledSeconds" -> Json.Null)
      )

  given Decoder[Error] = c =>
    c.downField("errorCode")
      .as[ErrorCode]
      .flatMap:
        case ErrorCode.SourceTooBright =>
          c.downField("wellHalfFilledSeconds").as[BigDecimal].map(SourceTooBright(_))
        case _                         =>
          c.downField("message").as[String].map(General(_))

case class ErrorExtension(targetIndex: Int, error: Error) derives Encoder.AsObject, Decoder

// sealed trait TargetExtension:
//   def targetIndex: Int

// case class GeneralExtension(targetIndex: Int) extends TargetExtension

// object GeneralExtension:
//   given Encoder.AsObject[GeneralExtension] = r =>
//     JsonObject(
//       "targetIndex" -> r.targetIndex.asJson
//     )

//   given Decoder[GeneralExtension] = c =>
//     for {
//       targetIndex <- c.downField("targetIndex").as[Int]
//     } yield GeneralExtension(targetIndex)

// sealed trait CodeExtension(val error: Error) extends TargetExtension

// object CodeExtension:
//   given Encoder.AsObject[CodeExtension] = r =>
//     JsonObject(
//       "errorCode" -> r.error.code.asJson
//     )

// case class SourceTooBrightExtension(wellHalfFilledSeconds: BigDecimal, targetIndex: Int)
//     extends CodeExtension(Error.SourceTooBright(wellHalfFilledSeconds))

// object SourceTooBrightExtension:
//   given Encoder.AsObject[SourceTooBrightExtension] = r =>
//     JsonObject(
//       "errorCode"   -> r.error.code.asJson,
//       "targetIndex" -> r.targetIndex.asJson,
//       "error"       -> Json.obj("wellHalfFilledSeconds" -> r.wellHalfFilledSeconds.asJson)
//     )

//   given Decoder[SourceTooBrightExtension] = c =>
//     for {
//       _           <- c.downField("errorCode")
//                        .as[ErrorCode]
//                        .flatMap:
//                          case ErrorCode.SourceTooBright => Right(ErrorCode.SourceTooBright)
//       // When we add more Extensions we'd need to handdle that case as:
//       // case _ => Left(DecodingFailure("SourceTooBrightExtension", c.history))
//       targetIndex <- c.downField("targetIndex").as[Int]
//       hw          <- c.downField("error").downField("wellHalfFilledSeconds").as[BigDecimal]
//     } yield SourceTooBrightExtension(hw, targetIndex)
