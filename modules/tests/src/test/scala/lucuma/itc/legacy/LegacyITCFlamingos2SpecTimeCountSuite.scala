// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.legacy

import io.circe.syntax.*
import lucuma.core.enums.*
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.core.util.Enumerated
import lucuma.itc.legacy.codecs.given
import lucuma.itc.service.ItcObservationDetails
import lucuma.itc.service.ObservingMode
import munit.FunSuite

import scala.concurrent.duration.*

/**
 * This is a unit test mostly to ensure all possible combination of params can be parsed by the
 * legacy ITC (Note that the ITC may still return an error but we want to ensure it can parse the
 * values
 */
class LegacyITCFlamingos2SpecTimeAndCountSuite extends FunSuite with CommonITCLegacySuite:
  override def munitTimeout: Duration = 5.minute

  override val obs = ItcObservationDetails(
    calculationMethod = ItcObservationDetails.CalculationMethod.S2NMethod.SpectroscopyS2N(
      exposureCount = 10,
      exposureDuration = 3.seconds,
      wavelengthAt = Wavelength.decimalNanometers.getOption(1200).get,
      coadds = None,
      sourceFraction = 1.0,
      ditherOffset = Angle.Angle0
    ),
    analysisMethod = lsAnalysisMethod
  )

  val f2Conf =
    ObservingMode.SpectroscopyMode.Flamingos2(
      F2Disperser.R3000,
      F2Filter.J,
      F2Fpu.LongSlit2
    )

  override val instrument = ItcInstrumentDetails(f2Conf)

  // Testing observing conditions
  testConditions("F2 spectroscopy TxC", baseParams)

  test("instrument filter".tag(LegacyITCTest)):
    Enumerated[F2Filter].all.foreach: f =>
      val d      = f match
        case F2Filter.J | F2Filter.H | F2Filter.JH | F2Filter.HK =>
          F2Disperser.R1200JH
        case _                                                   => F2Disperser.R3000
      val result = localItc
        .calculateIntegrationTime(
          bodyConf(sourceDefinition, obs, f2Conf.copy(filter = f, disperser = d)).asJson.noSpaces
        )
      assert(result.fold(allowedErrors, containsValidResults))

  test("instrument fpu".tag(LegacyITCTest)):
    Enumerated[F2Fpu].all.foreach: f =>
      val result = localItc
        .calculateIntegrationTime(
          bodyConf(sourceDefinition, obs, f2Conf.copy(fpu = f)).asJson.noSpaces
        )
      assert(result.fold(allowedErrors, containsValidResults))

  // Testing various SEDs
  testSEDs("F2 spectroscopy TxC", baseParams)

  // Testing user defined SED
  testUserDefinedSED("F2 spectroscopy TxC", baseParams)

  // Testing brightness units
  testBrightnessUnits("F2 spectroscopy TxC", baseParams)

  // Testing power law and blackbody
  testPowerAndBlackbody("F2 spectroscopy TxC", baseParams)
