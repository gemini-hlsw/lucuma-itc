// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import cats.syntax.all._
import eu.timepit.refined._
import eu.timepit.refined.numeric.NonNegative
import eu.timepit.refined.types.numeric.NonNegInt
import io.circe.CursorOp
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Encoder
import io.circe.HCursor
import io.circe.Json
import io.circe.generic.semiauto._
import io.circe.refined._
import lucuma.core.syntax.string._
import lucuma.core.util.Enumerated
import lucuma.itc.math._

import scala.util.Try

enum SeriesDataType(val tag: String):
  case SignalData     extends SeriesDataType("signal_data")
  case BackgroundData extends SeriesDataType("background_data")
  case SingleS2NData  extends SeriesDataType("single_s2_ndata")
  case FinalS2NData   extends SeriesDataType("final_s2_ndata")
  case PixSigData     extends SeriesDataType("pix_sig_data")
  case PixBackData    extends SeriesDataType("pix_back_data")

object SeriesDataType:
  given Enumerated[SeriesDataType] =
    Enumerated
      .from(
        SeriesDataType.SignalData,
        SeriesDataType.BackgroundData,
        SeriesDataType.SingleS2NData,
        SeriesDataType.FinalS2NData,
        SeriesDataType.PixSigData,
        SeriesDataType.PixBackData
      )
      .withTag(_.tag)

  val ocs2Decoder: Decoder[SeriesDataType] = (c: HCursor) =>
    Decoder.decodeJsonObject(c).flatMap { str =>
      val key = str.keys.headOption.orEmpty
      Try(SeriesDataType.valueOf(key)).toEither.leftMap { _ =>
        DecodingFailure(s"no enum value matched for $key", List(CursorOp.Field(key)))
      }
    }

final case class ItcAxis(start: Double, end: Double, min: Double, max: Double, count: Int)
    derives Decoder,
      Encoder.AsObject

final case class ItcSeries private (
  title:    String,
  dataType: SeriesDataType,
  data:     List[(Double, Double)],
  dataX:    List[Double],
  dataY:    List[Double],
  xAxis:    Option[ItcAxis],
  yAxis:    Option[ItcAxis]
) derives Encoder.AsObject

object ItcSeries:
  def apply(title: String, dataType: SeriesDataType, data: List[(Double, Double)]): ItcSeries =
    ItcSeries(title,
              dataType,
              data,
              data.map(_._1),
              data.map(_._2),
              calcAxis(data, _._1),
              calcAxis(data, _._2)
    )

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
