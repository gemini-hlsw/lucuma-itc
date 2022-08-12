// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import cats.syntax.all._
import io.circe.CursorOp
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Encoder
import io.circe.HCursor
import io.circe.Json
import io.circe.generic.semiauto._
import lucuma.core.syntax.string._
import lucuma.core.util.Enumerated

import scala.util.Try

enum SeriesDataType:
  case SignalData, BackgroundData, SingleS2NData, FinalS2NData, PixSigData, PixBackData

object SeriesDataType:
  given Enumerated[SeriesDataType] =
    Enumerated.of(
      SeriesDataType.SignalData,
      SeriesDataType.BackgroundData,
      SeriesDataType.SingleS2NData,
      SeriesDataType.FinalS2NData,
      SeriesDataType.PixSigData,
      SeriesDataType.PixBackData
    )

  val ocs2Decoder: Decoder[SeriesDataType] = (c: HCursor) =>
    Decoder.decodeJsonObject(c).flatMap { str =>
      val key = str.keys.headOption.orEmpty
      Try(SeriesDataType.valueOf(key)).toEither.leftMap { _ =>
        DecodingFailure(s"no enum value matched for $key", List(CursorOp.Field(key)))
      }
    }

final case class ItcSeries(title: String, dataType: SeriesDataType, data: List[(Double, Double)])
    derives Encoder.AsObject

object ItcSeries:
  val ocs2Decoder: Decoder[ItcSeries] = (c: HCursor) => {
    given Decoder[SeriesDataType] = SeriesDataType.ocs2Decoder
    for
      title <- c.downField("title").as[String]
      dt    <- c.downField("dataType").as[SeriesDataType]
      data  <- c.downField("data")
                 .as[List[List[Double]]]
                 .map { i =>
                   (i.lift(0), i.lift(1)) match
                     case (Some(a), Some(b)) if a.length === b.length => a.zip(b)
                     case _                                           => List.empty
                 }
    yield ItcSeries(title, dt, data)
  }

final case class ItcChart(series: List[ItcSeries]) derives Encoder.AsObject

object ItcChart:
  val ocs2Decoder: Decoder[ItcChart] = (c: HCursor) => {
    given Decoder[ItcSeries] = ItcSeries.ocs2Decoder
    c.downField("charts").downArray.downField("series").as[List[ItcSeries]].map(ItcChart.apply)
  }
