// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import cats.syntax.all._
import coulomb._
import coulomb.refined._
import coulomb.si.Kelvin
import eu.timepit.refined.numeric.Positive
import io.circe.Json
import io.circe.syntax._
import io.gatling.core.Predef._
import io.gatling.http.Predef._
import io.gatling.http.funspec.GatlingHttpFunSpec
import lucuma.core.enum._
import lucuma.core.math.Angle
import lucuma.core.math.Redshift
import lucuma.core.math.Wavelength
import lucuma.core.util.Enumerated
import lucuma.itc.search.ObservingMode
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.UnnormalizedSED
import scala.collection.immutable.SortedMap
import lucuma.core.math.BrightnessUnits._
import lucuma.core.math.BrightnessValue
import lucuma.core.math.dimensional._
import lucuma.core.math.units._

/**
 * This is a unit test mostly to ensure all possible combination of params can be parsed by the
 * legacy ITC (Note that the ITC may still return an error but we want to ensure it can parse the
 * values
 */
class LegacyITCSimulation extends GatlingHttpFunSpec {
  val headers_10 = Map("Content-Type" -> """application/json""")
  val baseUrl    = "https://gemini-new-itc.herokuapp.com"

  val sourceDefinition = ItcSourceDefinition(
    SourceProfile.Point(
      SpectralDefinition.BandNormalized(
        // UnnormalizedSED.BlackBody(BigDecimal(50.1).withRefinedUnit[Positive, Kelvin])
        UnnormalizedSED.StellarLibrary(StellarLibrarySpectrum.A0V),
        SortedMap(
          Band.J  -> BrightnessValue(5).withUnit[VegaMagnitude].toMeasureTagged,
          Band.R -> VegaMagnitudeIsIntegratedBrightnessUnit.unit.withValueTagged(
            BrightnessValue(5)
          )
        )
      )
    ),
    Band.R,
    Redshift(0.1)
  )
  val obs              = ItcObservationDetails(
    calculationMethod = ItcObservationDetails.CalculationMethod.SignalToNoise.Spectroscopy(
      exposures = 1,
      coadds = None,
      exposureDuration = 1,
      sourceFraction = 1.0,
      ditherOffset = Angle.Angle0
    ),
    analysisMethod = ItcObservationDetails.AnalysisMethod.Aperture.Auto(5)
  )

  val telescope  = ItcTelescopeDetails(
    wfs = ItcWavefrontSensor.OIWFS
  )
  val instrument = ItcInstrumentDetails.fromObservingMode(
    ObservingMode.Spectroscopy.GmosNorth(Wavelength.decimalNanometers.getOption(600).get,
                                         GmosNorthDisperser.B1200_G5301,
                                         GmosNorthFpu.LongSlit_5_00,
                                         none
    )
  )

  val conditions = ItcObservingConditions(ImageQuality.PointEight,
                                          CloudExtinction.OnePointFive,
                                          WaterVapor.Median,
                                          SkyBackground.Bright,
                                          2
  )

  def bodyCond(c: ItcObservingConditions) =
    ItcParameters(
      sourceDefinition,
      obs,
      c,
      telescope,
      instrument
    )

  Enumerated[ImageQuality].all.map { iq =>
    spec {
      http("sanity_cond_iq")
        .post("/json")
        .headers(headers_10)
        .check(status.in(200))
        .check(substring("decode").notExists)
        .check(substring("ItcSpectroscopyResult").exists)
        .body(StringBody(bodyCond(conditions.copy(iq = iq)).asJson.noSpaces))
    }
  }

  Enumerated[CloudExtinction].all.map { ce =>
    spec {
      http("sanity_cond_ce")
        .post("/json")
        .headers(headers_10)
        .check(status.in(200))
        .check(substring("decode").notExists)
        .check(substring("ItcSpectroscopyResult").exists)
        .body(StringBody(bodyCond(conditions.copy(cc = ce)).asJson.noSpaces))
    }
  }

  Enumerated[WaterVapor].all.map { wv =>
    spec {
      http("sanity_cond_wv")
        .post("/json")
        .headers(headers_10)
        .check(status.in(200))
        .check(substring("decode").notExists)
        .check(substring("ItcSpectroscopyResult").exists)
        .body(StringBody(bodyCond(conditions.copy(wv = wv)).asJson.noSpaces))
    }
  }

  Enumerated[SkyBackground].all.map { sb =>
    spec {
      http("sanity_cond_sb")
        .post("/json")
        .headers(headers_10)
        .check(status.in(200))
        .check(substring("ItcSpectroscopyResult").exists)
        .check(substring("decode").notExists)
        .body(StringBody(bodyCond(conditions.copy(sb = sb)).asJson.noSpaces))
    }
  }

  val gnConf = ObservingMode.Spectroscopy.GmosNorth(Wavelength.decimalNanometers.getOption(60).get,
                                                    GmosNorthDisperser.B1200_G5301,
                                                    GmosNorthFpu.Ifu2Slits,
                                                    none
  )

  val gsConf = ObservingMode.Spectroscopy.GmosSouth(Wavelength.decimalNanometers.getOption(60).get,
                                                    GmosSouthDisperser.B1200_G5321,
                                                    GmosSouthFpu.Ifu2Slits,
                                                    none
  )

  def bodyConf(c: ObservingMode.Spectroscopy) =
    ItcParameters(
      sourceDefinition,
      obs,
      ItcObservingConditions(ImageQuality.PointEight,
                             CloudExtinction.OnePointFive,
                             WaterVapor.Median,
                             SkyBackground.Dark,
                             2
      ),
      telescope,
      ItcInstrumentDetails.fromObservingMode(c)
    )

  // Enumerated[GmosNorthDisperser].all.map { d =>
  //   spec {
  //     http("sanity_gn_disperser")
  //       .post("/json")
  //       .headers(headers_10)
  //       .check(status.in(200, 400))
  //       .check(substring("decode").notExists)
  //       .body(StringBody(bodyConf(gnConf.copy(disperser = d)).asJson.noSpaces))
  //   }
  // }
  //
  // Enumerated[GmosNorthFpu].all.map { f =>
  //   spec {
  //     http("sanity_gn_fpu")
  //       .post("/json")
  //       .headers(headers_10)
  //       .check(status.in(200, 400))
  //       .check(substring("decode").notExists)
  //       .body(StringBody(bodyConf(gnConf.copy(fpu = f)).asJson.noSpaces))
  //   }
  // }
  //
  // Enumerated[GmosNorthFilter].all.map { f =>
  //   spec {
  //     http("sanity_gn_filter")
  //       .post("/json")
  //       .headers(headers_10)
  //       .check(status.in(200, 400))
  //       .check(substring("decode").notExists)
  //       .body(StringBody(bodyConf(gnConf.copy(filter = f.some)).asJson.noSpaces))
  //   }
  // }
  //
  // Enumerated[GmosSouthDisperser].all.map { d =>
  //   spec {
  //     http("sanity_gs_disperser")
  //       .post("/json")
  //       .headers(headers_10)
  //       .check(status.in(200, 400))
  //       .check(substring("decode").notExists)
  //       .body(StringBody(bodyConf(gsConf.copy(disperser = d)).asJson.noSpaces))
  //   }
  // }
  //
  // Enumerated[GmosSouthFpu].all.filter(_ =!= GmosSouthFpu.Bhros).map { f =>
  //   spec {
  //     http("sanity_gs_fpu")
  //       .post("/json")
  //       .headers(headers_10)
  //       .check(status.in(200, 400))
  //       .check(substring("decode").notExists)
  //       .body(StringBody(bodyConf(gsConf.copy(fpu = f)).asJson.noSpaces))
  //   }
  // }
  //
  // Enumerated[GmosSouthFilter].all.map { f =>
  //   spec {
  //     http("sanity_gn_filter")
  //       .post("/json")
  //       .headers(headers_10)
  //       .check(status.in(200, 400))
  //       .check(substring("decode").notExists)
  //       .body(StringBody(bodyConf(gsConf.copy(filter = f.some)).asJson.noSpaces))
  //   }
  // }
  //
  def bodySED(c: UnnormalizedSED) =
    ItcParameters(
      sourceDefinition.copy(profile =
        SourceProfile.unnormalizedSED
          .modifyOption(_ => c)(sourceDefinition.profile)
          .getOrElse(sourceDefinition.profile)
      ),
      obs,
      ItcObservingConditions(ImageQuality.PointEight,
                             CloudExtinction.OnePointFive,
                             WaterVapor.Median,
                             SkyBackground.Dark,
                             2
      ),
      telescope,
      instrument
    )

  // Enumerated[StellarLibrarySpectrum].all.map { f =>
  //   println(bodySED(UnnormalizedSED.StellarLibrary(f)).asJson.spaces2)
  //   spec {
  //     http("stellar_library")
  //       .post("/json")
  //       .headers(headers_10)
  //       .check(status.in(200, 400))
  //       .check(substring("decode").notExists)
  //       .body(StringBody(bodySED(UnnormalizedSED.StellarLibrary(f)).asJson.noSpaces))
  //   }
  // }

  // Enumerated[CoolStarTemperature].all.map { f =>
  //   println(bodySED(UnnormalizedSED.CoolStarModel(f)).asJson.spaces2)
  //   spec {
  //     http("cool_star")
  //       .post("/json")
  //       .headers(headers_10)
  //       .check(status.in(200, 400))
  //       .check(substring("decode").notExists)
  //       .check(substring("ItcSpectroscopyResult").exists)
  //       .body(StringBody(bodySED(UnnormalizedSED.CoolStarModel(f)).asJson.noSpaces))
  //   }
  // }
  //
  // Enumerated[GalaxySpectrum].all.map { f =>
  //   spec {
  //     http("galaxy")
  //       .post("/json")
  //       .headers(headers_10)
  //       .check(status.in(200, 400))
  //       .check(substring("decode").notExists)
  //       .body(StringBody(bodySED(UnnormalizedSED.Galaxy(f)).asJson.noSpaces))
  //   }
  // }
}
