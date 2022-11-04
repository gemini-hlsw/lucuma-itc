// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.tests

import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.applicative.*
import eu.timepit.refined.types.numeric.PosLong
import lucuma.core.math.Wavelength
import lucuma.core.model.NonNegDuration
import lucuma.itc.ChartType
import lucuma.itc.Itc
import lucuma.itc.ItcCcd
import lucuma.itc.ItcChart
import lucuma.itc.ItcChartGroup
import lucuma.itc.ItcObservingConditions
import lucuma.itc.ItcSeries
import lucuma.itc.SeriesDataType
import lucuma.itc.SignalToNoiseCalculation
import lucuma.itc.search.ObservingMode
import lucuma.itc.search.TargetProfile
import scala.concurrent.duration._


object FixedItc extends Itc[IO] with SignalToNoiseCalculation[IO] {

  override def calculateExposureTime(
    targetProfile:   TargetProfile,
    observingMode:   ObservingMode,
    constraints:     ItcObservingConditions,
    signalToNoise:   BigDecimal,
    signalToNoiseAt: Option[Wavelength]
  ): IO[Itc.CalcResultWithVersion] =
    Itc.CalcResultWithVersion(Itc.CalcResult.Success(1.seconds, 10, 10)).pure[IO]

  override def calculateGraph(
    targetProfile: TargetProfile,
    observingMode: ObservingMode,
    constraints:   ItcObservingConditions,
    exposureTime:  NonNegDuration,
    exposures:     PosLong
  ): IO[Itc.GraphResult] =
    Itc
      .GraphResult(
        "1",
        NonEmptyList.of(
          ItcCcd(1,
            1,
            2,
            2,
            Wavelength.fromNanometers(1001).get,
            Wavelength.fromNanometers(1001).get,
            3,
            4,
            5,
            Nil
          )
        ),
        NonEmptyList.of(
          ItcChartGroup(
            NonEmptyList.of(
              ItcChart(
                ChartType.S2NChart,
                List(
                  ItcSeries("title",
                    SeriesDataType.FinalS2NData,
                    List((1.0, 1000.0), (2.0, 1001.0))
                  )
                )
              )
            )
          )
        )
      )
      .pure[IO]

  override def itcVersions = IO.pure("versionToken")
}