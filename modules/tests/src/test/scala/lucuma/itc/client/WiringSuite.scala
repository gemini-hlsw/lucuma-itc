// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client

import buildinfo.BuildInfo
import cats.data.NonEmptyList
import cats.syntax.either.*
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.syntax.*
import lucuma.core.enums.Band
import lucuma.core.enums.CloudExtinction
import lucuma.core.enums.GalaxySpectrum.Spiral
import lucuma.core.enums.GmosAmpCount
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.enums.ImageQuality
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
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.core.util.TimeSpan
import lucuma.itc.ChartType
import lucuma.itc.FinalSN
import lucuma.itc.IntegrationTime
import lucuma.itc.ItcAxis
import lucuma.itc.ItcCcd
import lucuma.itc.SeriesDataType
import lucuma.itc.SingleSN
import lucuma.itc.client.json.encoders.given
import lucuma.itc.service.ItcMapping.versionDateTimeFormatter
import lucuma.refined.*

import java.time.Instant
import scala.collection.immutable.SortedMap

class WiringSuite extends ClientSuite {

  test("ItcClient spectroscopy basic wiring and sanity check") {
    spectroscopy(
      WiringSuite.SpectroscopyInput,
      IntegrationTimeResult(
        ItcVersions(
          versionDateTimeFormatter.format(Instant.ofEpochMilli(buildinfo.BuildInfo.buildDateTime)),
          BuildInfo.ocslibHash.some
        ),
        NonEmptyList
          .one(
            IntegrationTime(
              TimeSpan.FromString.getOption("PT1S").get,
              PosInt.unsafeFrom(10),
              SignalToNoise.unsafeFromBigDecimalExact(BigDecimal(10.0))
            )
          )
      ).asRight
    )
  }

  test("ItcClient imaging basic wiring and sanity check") {
    imaging(
      WiringSuite.ImagingInput,
      IntegrationTimeResult(
        ItcVersions(
          versionDateTimeFormatter.format(Instant.ofEpochMilli(buildinfo.BuildInfo.buildDateTime)),
          BuildInfo.ocslibHash.some
        ),
        NonEmptyList
          .one(
            IntegrationTime(
              TimeSpan.FromString.getOption("PT1S").get,
              PosInt.unsafeFrom(10),
              SignalToNoise.unsafeFromBigDecimalExact(BigDecimal(10.0))
            )
          )
      ).asRight
    )
  }

  test("SignalToNoiseAt null is removed") {
    WiringSuite.SpectroscopyInput.asJson.asObject
      .exists(!_.contains("signalToNoiseAt"))
  }

  test("SignalToNoiseAt non-null is included") {
    WiringSuite.SpectroscopyInput
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
                                    List(1000.0, 1001.0),
                                    ItcAxis(1, 2, 1, 2, 2).some,
                                    ItcAxis(1000.0, 1001.0, 1000, 1001, 2).some
              )
            )
          )
        ),
        FinalSN(SignalToNoise.unsafeFromBigDecimalExact(1009.0)),
        SignalToNoise.fromInt(1001).map(FinalSN(_)),
        SingleSN(SignalToNoise.unsafeFromBigDecimalExact(1003.0)),
        SignalToNoise.fromInt(1002).map(SingleSN(_))
      ).asRight
    )
  }
}

object WiringSuite {

  val SpectroscopyInput: SpectroscopyIntegrationTimeInput =
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
      InstrumentMode.GmosNorthSpectroscopy(
        GmosNorthGrating.B1200_G5301,
        GmosNorthFilter.GPrime.some,
        GmosFpu.North.builtin(GmosNorthFpu.LongSlit_0_25),
        GmosCcdMode(GmosXBinning.Two,
                    GmosYBinning.Two,
                    GmosAmpCount.Twelve,
                    GmosAmpGain.High,
                    GmosAmpReadMode.Fast
        ).some,
        GmosRoi.FullFrame.some
      )
    )

  val ImagingInput: ImagingIntegrationTimeInput =
    ImagingIntegrationTimeInput(
      Wavelength.Min,
      SignalToNoise.unsafeFromBigDecimalExact(BigDecimal(1)),
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
      InstrumentMode.GmosNorthImaging(
        GmosNorthFilter.GPrime
      )
    )

  val GraphInput: OptimizedSpectroscopyGraphInput =
    OptimizedSpectroscopyGraphInput(
      Wavelength.Min,
      Wavelength.fromIntMicrometers(1),
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
      InstrumentMode.GmosNorthSpectroscopy(
        GmosNorthGrating.B1200_G5301,
        GmosNorthFilter.GPrime.some,
        GmosFpu.North.builtin(GmosNorthFpu.LongSlit_0_25),
        GmosCcdMode(GmosXBinning.Two,
                    GmosYBinning.Two,
                    GmosAmpCount.Twelve,
                    GmosAmpGain.High,
                    GmosAmpReadMode.Fast
        ).some,
        GmosRoi.FullFrame.some
      ),
      Some(SignificantFiguresInput(2.refined, 2.refined, 2.refined))
    )
}
