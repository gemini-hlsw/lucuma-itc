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
import lucuma.core.enum.CloudExtinction
import lucuma.core.enum.GmosNorthDisperser
import lucuma.core.enum.GmosNorthFilter
import lucuma.core.enum.GmosNorthFpu
import lucuma.core.enum.GmosSouthDisperser
import lucuma.core.enum.GmosSouthFilter
import lucuma.core.enum.GmosSouthFpu
import lucuma.core.enum.ImageQuality
import lucuma.core.enum.SkyBackground
import lucuma.core.enum.WaterVapor
import lucuma.core.math.Angle
import lucuma.core.math.Redshift
import lucuma.core.math.Wavelength
import lucuma.core.math.units.Jansky
import lucuma.core.util.Enumerated
import lucuma.itc.ItcSourceDefinition._
import lucuma.itc.search.ObservingMode
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.UnnormalizedSED
import scala.collection.immutable.SortedMap
import lucuma.core.enum.Band
import lucuma.core.math.BrightnessUnits._
import lucuma.core.math.BrightnessValue
import lucuma.core.math.dimensional._
import lucuma.core.math.units._
import lucuma.core.enum.StellarLibrarySpectrum

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
          Band.Ap -> VegaMagnitudeIsIntegratedBrightnessUnit.unit.withValueTagged(
            BrightnessValue(5)
          )
        )
      )
    ),
    Band.Ap,
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
    ObservingMode.Spectroscopy.GmosNorth(Wavelength.decimalNanometers.getOption(60).get,
                                         GmosNorthDisperser.B1200_G5301,
                                         GmosNorthFpu.Ifu2Slits,
                                         none
    )
  )

  val conditions = ItcObservingConditions(ImageQuality.PointEight,
                                          CloudExtinction.OnePointFive,
                                          WaterVapor.Median,
                                          SkyBackground.Bright,
                                          2
  )

  def bodyCond(c: ItcObservingConditions) = Json.obj(
    "source"      -> sourceDefinition.asJson,
    "observation" -> obs.asJson,
    "conditions"  -> c.asJson,
    "telescope"   -> telescope.asJson,
    "instrument"  -> instrument.asJson
  )

  // Enumerated[ImageQuality].all.map { iq =>
  //   spec {
  //     http("sanity_cond_iq")
  //       .post("/json")
  //       .headers(headers_10)
  //       .check(status.in(200, 400))
  //       .check(substring("decode").notExists)
  //       .body(StringBody(bodyCond(conditions.copy(iq = iq)).noSpaces))
  //   }
  // }
  //
  // Enumerated[CloudExtinction].all.map { ce =>
  //   spec {
  //     http("sanity_cond_ce")
  //       .post("/json")
  //       .headers(headers_10)
  //       .check(status.in(200, 400))
  //       .check(substring("decode").notExists)
  //       .body(StringBody(bodyCond(conditions.copy(cc = ce)).noSpaces))
  //   }
  // }
  //
  // Enumerated[WaterVapor].all.map { wv =>
  //   spec {
  //     http("sanity_cond_wv")
  //       .post("/json")
  //       .headers(headers_10)
  //       .check(status.in(200, 400))
  //       .check(substring("decode").notExists)
  //       .body(StringBody(bodyCond(conditions.copy(wv = wv)).noSpaces))
  //   }
  // }
  //
  // Enumerated[SkyBackground].all.map { sb =>
  //   spec {
  //     http("sanity_cond_sb")
  //       .post("/json")
  //       .headers(headers_10)
  //       .check(status.in(200, 400))
  //       .check(substring("decode").notExists)
  //       .body(StringBody(bodyCond(conditions.copy(sb = sb)).noSpaces))
  //   }
  // }
  //
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
    ).asJson
  // Json.obj(
  // "source"      -> sourceDefinition.asJson,
  // "observation" -> obs.asJson,
  // "conditions"  -> ItcObservingConditions(ImageQuality.PointEight,
  //                                        CloudExtinction.OnePointFive,
  //                                        WaterVapor.Median,
  //                                        SkyBackground.Dark,
  //                                        2
  // ).asJson,
  // "telescope"   -> telescope.asJson,
  // "instrument"  -> ItcInstrumentDetails.fromObservingMode(c).asJson
  // )

  Enumerated[GmosNorthDisperser].all.map { d =>
    println(bodyConf(gnConf).spaces2)
    spec {
      http("sanity_gn_disperser")
        .post("/json")
        .headers(headers_10)
        .check(status.in(200, 400))
        .check(substring("decode").notExists)
        .body(StringBody(bodyConf(gnConf.copy(disperser = d)).noSpaces))
    }
  }

//   Enumerated[GmosNorthFpu].all.map { f =>
//     spec {
//       http("sanity_gn_fpu")
//         .post("/json")
//         .headers(headers_10)
//         .check(status.in(200, 400))
//         .check(substring("decode").notExists)
//         .body(StringBody(bodyConf(gnConf.copy(fpu = f)).noSpaces))
//     }
//   }
//
//   Enumerated[GmosNorthFilter].all.map { f =>
//     spec {
//       http("sanity_gn_filter")
//         .post("/json")
//         .headers(headers_10)
//         .check(status.in(200, 400))
//         .check(substring("decode").notExists)
//         .body(StringBody(bodyConf(gnConf.copy(filter = f.some)).noSpaces))
//     }
//   }
//
//   Enumerated[GmosSouthDisperser].all.map { d =>
//     spec {
//       http("sanity_gs_disperser")
//         .post("/json")
//         .headers(headers_10)
//         .check(status.in(200, 400))
//         .check(substring("decode").notExists)
//         .body(StringBody(bodyConf(gsConf.copy(disperser = d)).noSpaces))
//     }
//   }
//
//   Enumerated[GmosSouthFpu].all.filter(_ =!= GmosSouthFpu.Bhros).map { f =>
//     spec {
//       http("sanity_gs_fpu")
//         .post("/json")
//         .headers(headers_10)
//         .check(status.in(200, 400))
//         .check(substring("decode").notExists)
//         .body(StringBody(bodyConf(gsConf.copy(fpu = f)).noSpaces))
//     }
//   }
//
//   Enumerated[GmosSouthFilter].all.map { f =>
//     spec {
//       http("sanity_gn_filter")
//         .post("/json")
//         .headers(headers_10)
//         .check(status.in(200, 400))
//         .check(substring("decode").notExists)
//         .body(StringBody(bodyConf(gsConf.copy(filter = f.some)).noSpaces))
//     }
//   }
}
