// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import cats.implicits.*
import coulomb.*
import coulomb.syntax.*
import coulomb.units.si.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.syntax.*
import io.gatling.core.Predef.*
import io.gatling.http.Predef.*
import io.gatling.http.funspec.GatlingHttpFunSpec
import lucuma.core.enums.*
import lucuma.core.math.Angle
import lucuma.core.math.BrightnessUnits.*
import lucuma.core.math.BrightnessValue
import lucuma.core.math.Redshift
import lucuma.core.math.Wavelength
import lucuma.core.math.dimensional.*
import lucuma.core.math.units.*
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.UnnormalizedSED
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.core.util.Enumerated
import lucuma.itc.legacy.*
import lucuma.itc.legacy.given
import lucuma.itc.search.GmosNorthFpuParam
import lucuma.itc.search.GmosSouthFpuParam
import lucuma.itc.search.ItcObservationDetails
import lucuma.itc.search.ObservingMode
import lucuma.itc.search.syntax.*
import lucuma.refined.*

import scala.collection.immutable.SortedMap

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
        UnnormalizedSED.StellarLibrary(StellarLibrarySpectrum.A0V).some,
        SortedMap(
          Band.R -> BrightnessValue
            .unsafeFrom(5)
            .withUnit[VegaMagnitude]
            .toMeasureTagged
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
    ObservingMode.SpectroscopyMode.GmosNorth(
      Wavelength.decimalNanometers.getOption(600).get,
      GmosNorthGrating.B1200_G5301,
      GmosNorthFpuParam(GmosNorthFpu.LongSlit_5_00),
      none,
      GmosCcdMode(GmosXBinning.Two,
                  GmosYBinning.Two,
                  GmosAmpCount.Twelve,
                  GmosAmpGain.High,
                  GmosAmpReadMode.Fast
      ).some,
      GmosRoi.FullFrame.some
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

  val gnConf = ObservingMode.SpectroscopyMode.GmosNorth(
    Wavelength.decimalNanometers.getOption(600).get,
    GmosNorthGrating.B1200_G5301,
    GmosNorthFpuParam(GmosNorthFpu.LongSlit_1_00),
    none,
    GmosCcdMode(GmosXBinning.Two,
                GmosYBinning.Two,
                GmosAmpCount.Twelve,
                GmosAmpGain.High,
                GmosAmpReadMode.Fast
    ).some,
    GmosRoi.FullFrame.some
  )

  val gsConf = ObservingMode.SpectroscopyMode.GmosSouth(
    Wavelength.decimalNanometers.getOption(600).get,
    GmosSouthGrating.B1200_G5321,
    GmosSouthFpuParam(GmosSouthFpu.LongSlit_1_00),
    none,
    GmosCcdMode(GmosXBinning.Two,
                GmosYBinning.Two,
                GmosAmpCount.Twelve,
                GmosAmpGain.High,
                GmosAmpReadMode.Fast
    ).some,
    GmosRoi.FullFrame.some
  )

  def bodyConf(
    c:        ObservingMode.SpectroscopyMode,
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

  Enumerated[GmosNorthGrating].all.map { d =>
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

  Enumerated[GmosSouthGrating].all.map { d =>
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
        if (f.isGSIfu)
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
          .modifyOption(_ => c.some)(sourceDefinition.profile)
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
              f.withValueTagged(BrightnessValue.unsafeFrom(5))
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
            UnnormalizedSED.StellarLibrary(StellarLibrarySpectrum.A0V).some,
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
              f.withValueTagged(BrightnessValue.unsafeFrom(5))
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
            UnnormalizedSED.StellarLibrary(StellarLibrarySpectrum.A0V).some,
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
              f.withValueTagged(BrightnessValue.unsafeFrom(5))
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
            UnnormalizedSED.PowerLaw(c).some,
            SortedMap(
              Band.R -> BrightnessValue
                .unsafeFrom(5)
                .withUnit[VegaMagnitude]
                .toMeasureTagged
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

  def bodyBlackBody(c: PosInt) =
    ItcParameters(
      sourceDefinition.copy(profile =
        SourceProfile.Gaussian(
          Angle.fromDoubleArcseconds(10),
          SpectralDefinition.BandNormalized(
            UnnormalizedSED.BlackBody(c.withUnit[Kelvin]).some,
            SortedMap(
              Band.R -> BrightnessValue
                .unsafeFrom(5)
                .withUnit[VegaMagnitude]
                .toMeasureTagged
            )
          )
        )
      ),
      obs,
      conditions,
      telescope,
      instrument
    )

  List[PosInt](10.refined, 100.refined).map { f =>
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
