// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client

import cats.data.NonEmptyList
import cats.syntax.traverse.*
import io.circe.Encoder
import lucuma.core.math.SignalToNoise
import lucuma.itc.*

// These are limited versions of the chart for the client as we don't want to transfer all the data
case class OptimizedSeriesResult(
  title:      String,
  seriesType: SeriesDataType,
  dataY:      List[Double],
  xAxis:      Option[ItcAxis],
  yAxis:      Option[ItcAxis]
) derives Encoder.AsObject

case class OptimizedChartResult(chartType: ChartType, series: List[OptimizedSeriesResult])
    derives Encoder.AsObject

case class OptimizedSpectroscopyGraphResult(
  serverVersion:             String,
  dataVersion:               String,
  ccds:                      NonEmptyList[ItcCcd],
  charts:                    NonEmptyList[OptimizedChartResult],
  peakFinalSNRatio:          FinalSN,
  atWavelengthFinalSNRatio:  Option[FinalSN],
  peakSingleSNRatio:         SingleSN,
  atWavelengthSingleSNRatio: Option[SingleSN]
) derives Encoder.AsObject
