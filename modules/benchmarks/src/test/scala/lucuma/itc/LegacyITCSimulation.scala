// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import cats.implicits._
import coulomb._
import coulomb.si.Kelvin
import eu.timepit.refined.auto._
import eu.timepit.refined.types.numeric.PosBigDecimal
import io.circe.syntax._
import io.gatling.core.Predef._
import io.gatling.http.Predef._
import io.gatling.http.funspec.GatlingHttpFunSpec
import lucuma.core.enum._
import lucuma.core.math.Angle
import lucuma.core.math.BrightnessUnits._
import lucuma.core.math.BrightnessValue
import lucuma.core.math.Redshift
import lucuma.core.math.Wavelength
import lucuma.core.math.dimensional._
import lucuma.core.math.units._
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.UnnormalizedSED
import lucuma.core.util.Enumerated
import lucuma.itc.search.ObservingMode
import lucuma.itc.search.syntax.gmossouthfpu._

import scala.collection.immutable.SortedMap
import lucuma.itc.search.GmosNorthFpuParam
import lucuma.itc.search.GmosSouthFpuParam

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
        UnnormalizedSED.StellarLibrary(StellarLibrarySpectrum.A0V),
        SortedMap(
          Band.R -> BrightnessValue(5).withUnit[VegaMagnitude].toMeasureTagged
        )
      )
    ),
    Band.R,
    Redshift(0.03)
  )

  val lsAnalysisMethod  = ItcObservationDetails.AnalysisMethod.Aperture.Auto(5)
  val ifuAnalysisMethod =
    ItcObservationDetails.AnalysisMethod.Ifu.Single(skyFibres = 250, offset = 5.0)

  val obs = ItcObservationDetails(
    calculationMethod = ItcObservationDetails.CalculationMethod.SignalToNoise.Spectroscopy(
      exposures = 1,
      coadds = None,
      exposureDuration = 1,
      sourceFraction = 1.0,
      ditherOffset = Angle.Angle0
    ),
    analysisMethod = lsAnalysisMethod
  )

  val telescope  = ItcTelescopeDetails(
    wfs = ItcWavefrontSensor.OIWFS
  )
  val instrument = ItcInstrumentDetails.fromObservingMode(
    ObservingMode.Spectroscopy.GmosNorth(Wavelength.decimalNanometers.getOption(600).get,
                                         GmosNorthDisperser.B1200_G5301,
                                         GmosNorthFpuParam(GmosNorthFpu.LongSlit_5_00),
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

  val gnConf = ObservingMode.Spectroscopy.GmosNorth(Wavelength.decimalNanometers.getOption(600).get,
                                                    GmosNorthDisperser.B1200_G5301,
                                                    GmosNorthFpuParam(GmosNorthFpu.LongSlit_1_00),
                                                    none
  )

  val gsConf = ObservingMode.Spectroscopy.GmosSouth(Wavelength.decimalNanometers.getOption(600).get,
                                                    GmosSouthDisperser.B1200_G5321,
                                                    GmosSouthFpuParam(GmosSouthFpu.LongSlit_1_00),
                                                    none
  )

  def bodyConf(
    c:        ObservingMode.Spectroscopy,
    analysis: ItcObservationDetails.AnalysisMethod = lsAnalysisMethod
  ) =
    ItcParameters(
      sourceDefinition,
      obs.copy(analysisMethod = analysis),
      ItcObservingConditions(ImageQuality.PointEight,
                             CloudExtinction.OnePointFive,
                             WaterVapor.Median,
                             SkyBackground.Dark,
                             2
      ),
      telescope,
      ItcInstrumentDetails.fromObservingMode(c)
    )

  Enumerated[GmosNorthDisperser].all.map { d =>
    spec {
      http("sanity_gn_disperser")
        .post("/json")
        .headers(headers_10)
        .check(status.in(200))
        .check(substring("decode").notExists)
        .check(substring("ItcSpectroscopyResult").exists)
        .body(StringBody(bodyConf(gnConf.copy(disperser = d)).asJson.noSpaces))
    }
  }

  Enumerated[GmosNorthFpu].all.map { f =>
    spec {
      http("sanity_gn_fpu")
        .post("/json")
        .headers(headers_10)
        .check(status.in(200, 400))
        .check(substring("decode").notExists)
        .body(StringBody(bodyConf(gnConf.copy(fpu = GmosNorthFpuParam(f))).asJson.noSpaces))
    }
  }

  Enumerated[GmosNorthFilter].all.map { f =>
    spec {
      http("sanity_gn_filter")
        .post("/json")
        .headers(headers_10)
        .check(status.in(200, 400))
        .check(substring("decode").notExists)
        // .check(substring("ItcSpectroscopyResult").exists)
        .body(StringBody(bodyConf(gnConf.copy(filter = f.some)).asJson.noSpaces))
    }
  }

  Enumerated[GmosSouthDisperser].all.map { d =>
    spec {
      http("sanity_gs_disperser")
        .post("/json")
        .headers(headers_10)
        .check(status.in(200))
        .check(substring("decode").notExists)
        .check(substring("ItcSpectroscopyResult").exists)
        .body(StringBody(bodyConf(gsConf.copy(disperser = d)).asJson.noSpaces))
    }
  }

  Enumerated[GmosSouthFpu].all
    .filter(f => f =!= GmosSouthFpu.Bhros)
    .map { f =>
      val conf =
        if (f.isIfu)
          bodyConf(gsConf.copy(fpu = GmosSouthFpuParam(f)), ifuAnalysisMethod)
        else
          bodyConf(gsConf.copy(fpu = GmosSouthFpuParam(f)))
      spec {
        http("sanity_gs_fpu")
          .post("/json")
          .headers(headers_10)
          .check(status.in(200))
          .check(substring("decode").notExists)
          .check(substring("ItcSpectroscopyResult").exists)
          .body(StringBody(conf.asJson.noSpaces))
      }
    }

  Enumerated[GmosSouthFilter].all.map { f =>
    spec {
      http("sanity_gn_filter")
        .post("/json")
        .headers(headers_10)
        .check(status.in(200, 400))
        .check(substring("decode").notExists)
        // .check(substring("ItcSpectroscopyResult").exists)
        .body(StringBody(bodyConf(gsConf.copy(filter = f.some)).asJson.noSpaces))
    }
  }

  def bodySED(c: UnnormalizedSED) =
    ItcParameters(
      sourceDefinition.copy(profile =
        SourceProfile.unnormalizedSED
          .modifyOption(_ => c)(sourceDefinition.profile)
          .getOrElse(sourceDefinition.profile)
      ),
      obs,
      conditions,
      telescope,
      instrument
    )

  Enumerated[StellarLibrarySpectrum].all.map { f =>
    spec {
      http("stellar_library")
        .post("/json")
        .headers(headers_10)
        .check(status.in(200))
        .check(substring("decode").notExists)
        .check(substring("ItcSpectroscopyResult").exists)
        .body(StringBody(bodySED(UnnormalizedSED.StellarLibrary(f)).asJson.noSpaces))
    }
  }

  Enumerated[CoolStarTemperature].all.map { f =>
    spec {
      http("cool_star")
        .post("/json")
        .headers(headers_10)
        .check(status.in(200))
        .check(substring("decode").notExists)
        .check(substring("ItcSpectroscopyResult").exists)
        .body(StringBody(bodySED(UnnormalizedSED.CoolStarModel(f)).asJson.noSpaces))
    }
  }

  Enumerated[GalaxySpectrum].all.map { f =>
    spec {
      http("galaxy")
        .post("/json")
        .headers(headers_10)
        .check(status.in(200))
        .check(substring("decode").notExists)
        .check(substring("ItcSpectroscopyResult").exists)
        .body(StringBody(bodySED(UnnormalizedSED.Galaxy(f)).asJson.noSpaces))
    }
  }

  Enumerated[PlanetSpectrum].all.map { f =>
    spec {
      http("planet")
        .post("/json")
        .headers(headers_10)
        .check(status.in(200))
        .check(substring("decode").notExists)
        .check(substring("ItcSpectroscopyResult").exists)
        .body(StringBody(bodySED(UnnormalizedSED.Planet(f)).asJson.noSpaces))
    }
  }

  Enumerated[QuasarSpectrum].all.map { f =>
    spec {
      http("quasar")
        .post("/json")
        .headers(headers_10)
        .check(status.in(200))
        .check(substring("decode").notExists)
        .check(substring("ItcSpectroscopyResult").exists)
        .body(StringBody(bodySED(UnnormalizedSED.Quasar(f)).asJson.noSpaces))
    }
  }

  Enumerated[HIIRegionSpectrum].all.map { f =>
    spec {
      http("hiiregion")
        .post("/json")
        .headers(headers_10)
        .check(status.in(200))
        .check(substring("decode").notExists)
        .check(substring("ItcSpectroscopyResult").exists)
        .body(StringBody(bodySED(UnnormalizedSED.HIIRegion(f)).asJson.noSpaces))
    }
  }

  Enumerated[PlanetaryNebulaSpectrum].all.map { f =>
    spec {
      http("quasar")
        .post("/json")
        .headers(headers_10)
        .check(status.in(200))
        .check(substring("decode").notExists)
        .check(substring("ItcSpectroscopyResult").exists)
        .body(StringBody(bodySED(UnnormalizedSED.PlanetaryNebula(f)).asJson.noSpaces))
    }
  }

  def bodyIntMagUnits(c: BrightnessMeasure[Integrated]) =
    ItcParameters(
      sourceDefinition.copy(profile =
        SourceProfile
          .integratedBrightnessIn(Band.R)
          .replace(
            c
          )(sourceDefinition.profile)
      ),
      obs,
      conditions,
      telescope,
      instrument
    )

  Brightness.Integrated.all.map { f =>
    spec {
      http("integrated units")
        .post("/json")
        .headers(headers_10)
        .check(status.in(200))
        .check(substring("decode").notExists)
        .check(substring("ItcSpectroscopyResult").exists)
        .body(
          StringBody(
            bodyIntMagUnits(
              f.withValueTagged(BrightnessValue(5))
            ).asJson.noSpaces
          )
        )
    }
  }

  def bodySurfaceMagUnits(c: BrightnessMeasure[Surface]) =
    ItcParameters(
      sourceDefinition.copy(profile =
        SourceProfile.Uniform(
          SpectralDefinition.BandNormalized(
            UnnormalizedSED.StellarLibrary(StellarLibrarySpectrum.A0V),
            SortedMap(
              Band.R -> c
            )
          )
        )
      ),
      obs,
      conditions,
      telescope,
      instrument
    )

  Brightness.Surface.all.map { f =>
    spec {
      http("surface units")
        .post("/json")
        .headers(headers_10)
        .check(status.in(200))
        .check(substring("decode").notExists)
        .check(substring("ItcSpectroscopyResult").exists)
        .body(
          StringBody(
            bodySurfaceMagUnits(
              f.withValueTagged(BrightnessValue(5))
            ).asJson.noSpaces
          )
        )
    }
  }

  def bodyIntGaussianMagUnits(c: BrightnessMeasure[Integrated]) =
    ItcParameters(
      sourceDefinition.copy(profile =
        SourceProfile.Gaussian(
          Angle.fromDoubleArcseconds(10),
          SpectralDefinition.BandNormalized(
            UnnormalizedSED.StellarLibrary(StellarLibrarySpectrum.A0V),
            SortedMap(
              Band.R -> c
            )
          )
        )
      ),
      obs,
      conditions,
      telescope,
      instrument
    )

  Brightness.Integrated.all.map { f =>
    spec {
      http("gaussian integrated units")
        .post("/json")
        .headers(headers_10)
        .check(status.in(200))
        .check(substring("decode").notExists)
        .check(substring("ItcSpectroscopyResult").exists)
        .body(
          StringBody(
            bodyIntGaussianMagUnits(
              f.withValueTagged(BrightnessValue(5))
            ).asJson.noSpaces
          )
        )
    }
  }

  def bodyPowerLaw(c: Int) =
    ItcParameters(
      sourceDefinition.copy(profile =
        SourceProfile.Gaussian(
          Angle.fromDoubleArcseconds(10),
          SpectralDefinition.BandNormalized(
            UnnormalizedSED.PowerLaw(c),
            SortedMap(
              Band.R -> BrightnessValue(5).withUnit[VegaMagnitude].toMeasureTagged
            )
          )
        )
      ),
      obs,
      conditions,
      telescope,
      instrument
    )

  List(-10, 0, 10, 100).map { f =>
    spec {
      http("power law")
        .post("/json")
        .headers(headers_10)
        .check(status.in(200))
        .check(substring("decode").notExists)
        .check(substring("ItcSpectroscopyResult").exists)
        .body(
          StringBody(
            bodyPowerLaw(f).asJson.noSpaces
          )
        )
    }
  }

  def bodyBlackBody(c: PosBigDecimal) =
    ItcParameters(
      sourceDefinition.copy(profile =
        SourceProfile.Gaussian(
          Angle.fromDoubleArcseconds(10),
          SpectralDefinition.BandNormalized(
            UnnormalizedSED.BlackBody(c.withUnit[Kelvin]),
            SortedMap(
              Band.R -> BrightnessValue(5).withUnit[VegaMagnitude].toMeasureTagged
            )
          )
        )
      ),
      obs,
      conditions,
      telescope,
      instrument
    )

  List[PosBigDecimal](BigDecimal(0.1), BigDecimal(10), BigDecimal(100)).map { f =>
    spec {
      http("black body")
        .post("/json")
        .headers(headers_10)
        .check(status.in(200))
        .check(substring("decode").notExists)
        .check(substring("ItcSpectroscopyResult").exists)
        .body(
          StringBody(
            bodyBlackBody(f).asJson.noSpaces
          )
        )
    }
  }

  // def bodyEmissionLine(c: PosBigDecimal) =
  //   ItcParameters(
  //     sourceDefinition.copy(profile =
  //       SourceProfile.Point(
  //         SpectralDefinition.EmissionLines(
  //           SortedMap(
  //             Wavelength.decimalNanometers.getOption(600).get -> EmissionLine(
  //               c.withUnit[KilometersPerSecond],
  //               c
  //                 .withUnit[WattsPerMeter2]
  //                 .toMeasureTagged
  //             )
  //           ),
  //           BigDecimal(5)
  //             .withRefinedUnit[Positive, WattsPerMeter2Micrometer]
  //             .toMeasureTagged
  //         )
  //       )
  //     ),
  //     obs,
  //     conditions,
  //     telescope,
  //     instrument
  //   )
  //
  // List[PosBigDecimal](BigDecimal(0.1), BigDecimal(10), BigDecimal(100)).map { f =>
  //   println(bodyEmissionLine(f).asJson)
  //   spec {
  //     http("emission line")
  //       .post("/json")
  //       .headers(headers_10)
  //       .check(status.in(200))
  //       .check(substring("decode").notExists)
  //       .check(substring("ItcSpectroscopyResult").exists)
  //       .body(
  //         StringBody(
  //           bodyEmissionLine(f).asJson.noSpaces
  //         )
  //       )
  //   }
  // }

}
