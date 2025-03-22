// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.legacy

import cats.syntax.option.* // For .some extension method
import io.circe.syntax.*
import lucuma.core.enums.*
import lucuma.core.math.Angle
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.core.util.Enumerated
import lucuma.itc.legacy.codecs.given
import lucuma.itc.service.ItcObservationDetails
import lucuma.itc.service.ObservingMode
import munit.FunSuite

import scala.concurrent.duration.*

/**
 * This is a unit test for F2 imaging mode in the legacy ITC, ensuring all possible combinations of
 * parameters can be parsed. The ITC may still return an error but we want to ensure it can parse
 * the values.
 */
class LegacyITCFlamingos2ImaSignalToNoiseSuite extends FunSuite with CommonITCLegacySuite:
  override def munitTimeout: Duration = 5.minute

  val analysisMethod = ItcObservationDetails.AnalysisMethod.Aperture.Auto(5)

  override val obs = ItcObservationDetails(
    calculationMethod =
      ItcObservationDetails.CalculationMethod.IntegrationTimeMethod.ImagingIntegrationTime(
        sigma = 600,
        coadds = None,
        sourceFraction = 1.0,
        ditherOffset = Angle.Angle0
      ),
    analysisMethod = analysisMethod
  )

  val gmosNConf = ObservingMode.ImagingMode.GmosNorth(
    GmosNorthFilter.GPrime,
    GmosCcdMode(
      GmosXBinning.One,
      GmosYBinning.One,
      GmosAmpCount.Twelve,
      GmosAmpGain.High,
      GmosAmpReadMode.Fast
    ).some
  )

  override val instrument = ItcInstrumentDetails(gmosNConf)

  val gmosSConf = ObservingMode.ImagingMode.GmosSouth(
    GmosSouthFilter.GPrime,
    GmosCcdMode(
      GmosXBinning.One,
      GmosYBinning.One,
      GmosAmpCount.Twelve,
      GmosAmpGain.High,
      GmosAmpReadMode.Fast
    ).some
  )

  // Testing observing conditions
  testConditions("GMOS imaging TxC", baseParams)

  test("gmos north filter".tag(LegacyITCTest)):
    Enumerated[GmosNorthFilter].all.foreach: f =>
      val result = localItc
        .calculateIntegrationTime(
          bodyConf(sourceDefinition,
                   obs,
                   gmosNConf.copy(filter = f),
                   analysisMethod
          ).asJson.noSpaces
        )
      assert(result.fold(allowedErrors, containsValidResults))

  test("gmos south filter".tag(LegacyITCTest)):
    Enumerated[GmosSouthFilter].all.foreach: f =>
      val result = localItc
        .calculateIntegrationTime(
          bodyConf(sourceDefinition,
                   obs,
                   gmosSConf.copy(filter = f),
                   analysisMethod
          ).asJson.noSpaces
        )
      assert(result.fold(allowedErrors, containsValidResults))

  // Testing various SEDs
  testSEDs("GMOS imaging TxC", baseParams, runStellar = false, runCoolStar = false)

  // Testing user defined SED
  testUserDefinedSED("GMOS imaging TxC", baseParams)

  // Testing brightness units
  testBrightnessUnits("GMOS imaging TxC", baseParams)

  // Testing power law and blackbody
  testPowerAndBlackbody("GMOS imaging TxC", baseParams)
