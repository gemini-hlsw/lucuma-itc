// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import cats.data.NonEmptyList
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
import lucuma.core.enums.*
import lucuma.core.syntax.string._
import lucuma.core.util.Enumerated
import lucuma.itc.math._

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

enum ChartType(val tag: String):
  case SignalChart extends ChartType("signal_chart")
  case S2NChart    extends ChartType("s2n_chart")

object ChartType:
  given Enumerated[ChartType] =
    Enumerated
      .from(
        ChartType.SignalChart,
        ChartType.S2NChart
      )
      .withTag(_.tag)

case class ItcAxis(start: Double, end: Double, min: Double, max: Double, count: Int)
    derives Decoder,
      Encoder.AsObject

object ItcAxis:
  // Calculate the values on the axis' range
  def calcAxis(data: List[(Double, Double)], fun: ((Double, Double)) => Double): Option[ItcAxis] =
    if (data.nonEmpty)
      val (min, max, count) =
        data.foldLeft((Double.MaxValue, Double.MinValue, 0)) { case ((max, min, count), current) =>
          val x = fun(current)
          (x.min(max), x.max(min), count + 1)
        }
      ItcAxis(fun(data.head), fun(data.last), min, max, count).some
    else none

case class ItcSeries private (
  title:      String,
  seriesType: SeriesDataType,
  data:       List[(Double, Double)],
  dataX:      List[Double],
  dataY:      List[Double],
  xAxis:      Option[ItcAxis],
  yAxis:      Option[ItcAxis]
) derives Encoder.AsObject

object ItcSeries:
  def apply(title: String, dataType: SeriesDataType, data: List[(Double, Double)]): ItcSeries =
    ItcSeries(title,
              dataType,
              data,
              data.map(_._1),
              data.map(_._2),
              ItcAxis.calcAxis(data, _._1),
              ItcAxis.calcAxis(data, _._2)
    )

case class ItcChart(chartType: ChartType, series: List[ItcSeries]) derives Encoder.AsObject

case class ItcChartGroup(charts: NonEmptyList[ItcChart]) derives Encoder.AsObject
