// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client

import cats.syntax.either.*
import cats.syntax.option.*
import eu.timepit.refined.auto.*
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.PosBigDecimal
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
import lucuma.core.math.Wavelength
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ElevationRange.AirMass
import lucuma.core.model.NonNegDuration
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition.BandNormalized
import lucuma.core.model.UnnormalizedSED.Galaxy
import lucuma.itc.client.SpectroscopyModeInput
import lucuma.itc.service.ItcMapping.versionDateTimeFormatter

import java.time.Duration
import java.time.Instant
import scala.collection.immutable.SortedMap

class WiringSuite extends ClientSuite {

  test("ItcClient spectroscopy basic wiring and sanity check") {
    spectroscopy(
      WiringSuite.Input,
      SpectroscopyResult(
        ItcVersions(
          versionDateTimeFormatter.format(Instant.ofEpochMilli(buildinfo.BuildInfo.buildDateTime)),
          None
        ),
        ItcResult
          .Success(
            NonNegDuration.unsafeFrom(Duration.parse("PT1S")),
            NonNegInt.unsafeFrom(10),
            PosBigDecimal.unsafeFrom(10.0)
          )
          .some
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

  test("ItcClient versions") {
    versions(
      ItcVersions(
        versionDateTimeFormatter.format(Instant.ofEpochMilli(buildinfo.BuildInfo.buildDateTime)),
        Some("versionToken")
      ).asRight
    )
  }

}

object WiringSuite {

  val Input: SpectroscopyModeInput =
    SpectroscopyModeInput(
      Wavelength.Min,
      PosBigDecimal.unsafeFrom(BigDecimal(1.0)),
      Option.empty[Wavelength],
      SourceProfile.Point(BandNormalized[Integrated](Galaxy(Spiral), SortedMap.empty)),
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

}
