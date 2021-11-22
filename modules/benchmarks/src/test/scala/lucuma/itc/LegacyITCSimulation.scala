// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import cats.syntax.all._
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
import lucuma.core.enum.ImageQuality
import lucuma.core.enum.MagnitudeBand
import lucuma.core.enum.MagnitudeSystem
import lucuma.core.enum.SkyBackground
import lucuma.core.enum.WaterVapor
import lucuma.core.math.Angle
import lucuma.core.math.MagnitudeValue
import lucuma.core.math.Redshift
import lucuma.core.math.Wavelength
import lucuma.core.model.SpatialProfile
import lucuma.core.model.SpectralDistribution
import lucuma.core.util.Enumerated
import lucuma.itc.ItcSourceDefinition._
import lucuma.itc.search.ObservingMode

/**
 * This is a unit test mostly to ensure all possible combination of params can be parsed by the
 * legacy ITC (Note that the ITC may still return an error but we want to ensure it can parse the
 * values
 */
class LegacyITCSimulation extends GatlingHttpFunSpec {
  val headers_10 = Map("Content-Type" -> """application/json""")
  val baseUrl    = "https://gemini-new-itc.herokuapp.com"

  val sourceDefinition = ItcSourceDefinition(
    SpatialProfile.PointSource,
    SpectralDistribution.BlackBody(BigDecimal(50.1).withRefinedUnit[Positive, Kelvin]),
    MagnitudeValue(5),
    MagnitudeSystem.Jy.asLeft,
    MagnitudeBand.Ap,
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

  val telescope                       = ItcTelescopeDetails(
    wfs = ItcWavefrontSensor.OIWFS
  )
  val instrument                      = ItcInstrumentDetails.fromObservingMode(
    ObservingMode.Spectroscopy.GmosNorth(Wavelength.decimalNanometers.getOption(60).get,
                                         GmosNorthDisperser.B1200_G5301,
                                         GmosNorthFpu.Ifu2Slits,
                                         GmosNorthFilter.GG455.some
    )
  )
  def body(c: ItcObservingConditions) = Json.obj(
    "source"      -> sourceDefinition.asJson,
    "observation" -> obs.asJson,
    "conditions"  -> c.asJson,
    "telescope"   -> telescope.asJson,
    "instrument"  -> instrument.asJson
  )

  val allConditions =
    Enumerated[ImageQuality].all
      .zip(Enumerated[CloudExtinction].all)
      .zip(Enumerated[WaterVapor].all)
      .zip(Enumerated[SkyBackground].all)
      .map { case (((iq, ce), wv), sb) =>
        ItcObservingConditions(iq, ce, wv, sb, 2)
      }

  allConditions.map { c =>
    spec {
      http("sanity")
        .post("/json")
        .headers(headers_10)
        .check(status.in(200, 400))
        .check(substring("decode").notExists)
        .body(StringBody(body(c).noSpaces))
    }
  }

}
