// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client

import buildinfo.BuildInfo
import cats.data.NonEmptyList
import cats.syntax.either.*
import cats.syntax.option.*
import eu.timepit.refined.auto.*
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.numeric.PosInt
import io.circe.syntax.*
import lucuma.core.enums.Band
import lucuma.core.enums.CloudExtinction
import lucuma.core.enums.GalaxySpectrum.Spiral
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.ImageQuality
import lucuma.core.enums.Instrument
import lucuma.core.enums.SkyBackground
import lucuma.core.enums.WaterVapor
import lucuma.core.math.BrightnessUnits.Integrated
import lucuma.core.math.RadialVelocity
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ElevationRange.AirMass
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition.BandNormalized
import lucuma.core.model.UnnormalizedSED.Galaxy
import lucuma.core.util.TimeSpan
import lucuma.itc.ChartType
import lucuma.itc.IntegrationTime
import lucuma.itc.ItcAxis
import lucuma.itc.ItcCcd
import lucuma.itc.ItcChart
import lucuma.itc.ItcChartGroup
import lucuma.itc.ItcSeries
import lucuma.itc.SeriesDataType
import lucuma.itc.client.json.encoders.given
import lucuma.itc.service.ItcMapping.versionDateTimeFormatter
import lucuma.refined.*

import java.time.Instant
import scala.collection.immutable.SortedMap

class WiringSuite extends ClientSuite {

  test("ItcClient spectroscopy basic wiring and sanity check") {
    spectroscopy(
      WiringSuite.Input,
      SpectroscopyResult(
        ItcVersions(
          versionDateTimeFormatter.format(Instant.ofEpochMilli(buildinfo.BuildInfo.buildDateTime)),
          BuildInfo.ocslibHash.some
        ),
        IntegrationTime(
          TimeSpan.FromString.getOption("PT1S").get,
          PosInt.unsafeFrom(10),
          SignalToNoise.unsafeFromBigDecimalExact(BigDecimal(10.0))
        ).some
      ).asRight
    )
  }

  test("SignalToNoiseAt null is removed") {
    WiringSuite.Input.asJson.asObject
      .exists(!_.contains("signalToNoiseAt"))
  }

  test("SignalToNoiseAt non-null is included") {
    import lucuma.itc.client.json.given
    WiringSuite.Input
      .copy(signalToNoiseAt = Wavelength.Min.some)
      .asJson
      .asObject
      .flatMap(_.apply("signalToNoiseAt"))
      .map(_.spaces2)
      .contains(Wavelength.Min.asJson)
  }

  test("ItcClient spectroscopy graph wiring and sanity check") {
    optimizedSpectroscopyGraph(
      WiringSuite.GraphInput,
      OptimizedSpectroscopyGraphResult(
        versionDateTimeFormatter.format(Instant.ofEpochMilli(buildinfo.BuildInfo.buildDateTime)),
        BuildInfo.ocslibHash,
        NonEmptyList.of(
          ItcCcd(1,
                 1,
                 2,
                 2,
                 Wavelength.fromIntNanometers(1001).get,
                 Wavelength.fromIntNanometers(1001).get,
                 3,
                 4,
                 5,
                 Nil
          )
        ),
        NonEmptyList.of(
          OptimizedChartResult(
            ChartType.S2NChart,
            List(
              OptimizedSeriesResult("title",
                                    SeriesDataType.FinalS2NData,
                                    List(1000.0, 1000.0),
                                    ItcAxis(1, 2, 1, 2, 2).some,
                                    ItcAxis(1000.0, 1000.0, 1000, 1000, 2).some
              )
            )
          )
        )
      ).asRight
    )
  }
}

object WiringSuite {

  val Input: SpectroscopyIntegrationTimeInput =
    SpectroscopyIntegrationTimeInput(
      Wavelength.Min,
      SignalToNoise.unsafeFromBigDecimalExact(BigDecimal(1)),
      Option.empty[Wavelength],
      SourceProfile.Point(BandNormalized[Integrated](Galaxy(Spiral).some, SortedMap.empty)),
      Band.SloanU,
      RadialVelocity.fromMetersPerSecond.getOption(1.0).get,
      ConstraintSet(
        ImageQuality.PointOne,
        CloudExtinction.PointOne,
        SkyBackground.Darkest,
        WaterVapor.VeryDry,
        AirMass.Default
      ),
      InstrumentMode.GmosNorth(
        GmosNorthGrating.B1200_G5301,
        GmosNorthFilter.GPrime.some,
        GmosFpu.North.builtin(GmosNorthFpu.LongSlit_0_25)
      )
    )

  val GraphInput: OptimizedSpectroscopyGraphInput =
    OptimizedSpectroscopyGraphInput(
      Wavelength.Min,
      TimeSpan.fromSeconds(1).get,
      PosInt.unsafeFrom(5),
      SourceProfile.Point(BandNormalized[Integrated](Galaxy(Spiral).some, SortedMap.empty)),
      Band.SloanU,
      RadialVelocity.fromMetersPerSecond.getOption(1.0).get,
      ConstraintSet(
        ImageQuality.PointOne,
        CloudExtinction.PointOne,
        SkyBackground.Darkest,
        WaterVapor.VeryDry,
        AirMass.Default
      ),
      InstrumentMode.GmosNorth(
        GmosNorthGrating.B1200_G5301,
        GmosNorthFilter.GPrime.some,
        GmosFpu.North.builtin(GmosNorthFpu.LongSlit_0_25)
      ),
      Some(SignificantFiguresInput(2.refined, 2.refined, 2.refined))
    )
}
