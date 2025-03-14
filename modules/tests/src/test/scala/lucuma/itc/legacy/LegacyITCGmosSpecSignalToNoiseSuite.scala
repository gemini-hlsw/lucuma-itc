// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.legacy

import cats.data.NonEmptyMap
import cats.implicits.*
import coulomb.*
import coulomb.syntax.*
import coulomb.units.si.*
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.numeric.PosInt
import io.circe.syntax.*
import lucuma.core.enums.*
import lucuma.core.math.Angle
import lucuma.core.math.BrightnessUnits.*
import lucuma.core.math.BrightnessValue
import lucuma.core.math.Redshift
import lucuma.core.math.Wavelength
import lucuma.core.math.dimensional.*
import lucuma.core.math.dimensional.syntax.*
import lucuma.core.math.units.*
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.UnnormalizedSED
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.core.util.Enumerated
import lucuma.itc.ItcObservingConditions
import lucuma.itc.legacy.given
import lucuma.itc.search.GmosNorthFpuParam
import lucuma.itc.search.GmosSouthFpuParam
import lucuma.itc.search.ItcObservationDetails
import lucuma.itc.search.ObservingMode
import lucuma.itc.search.TargetData
import munit.FunSuite

import scala.collection.immutable.SortedMap
import scala.concurrent.duration.*

/**
 * This is a unit test mostly to ensure all possible combination of params can be parsed by the
 * legacy ITC (Note that the ITC may still return an error but we want to ensure it can parse the
 * values
 */
class LegacyITCGmosSpecSignalToNoiseSuite extends FunSuite with CommonITCLegacySuite:
  override def munitTimeout: Duration = 5.minute

  val sourceDefinition = ItcSourceDefinition(
    TargetData(
      SourceProfile.Point(
        SpectralDefinition.BandNormalized(
          UnnormalizedSED.StellarLibrary(StellarLibrarySpectrum.A0V).some,
          SortedMap(
            Band.R -> BrightnessValue
              .unsafeFrom(9)
              .withUnit[VegaMagnitude]
              .toMeasureTagged
          )
        )
      ),
      Redshift(0.03)
    ),
    Band.R.asLeft
  )

  val lsAnalysisMethod  = ItcObservationDetails.AnalysisMethod.Aperture.Auto(5)
  val ifuAnalysisMethod =
    ItcObservationDetails.AnalysisMethod.Ifu.Single(skyFibres = 250, offset = 5.0)

  val obs = ItcObservationDetails(
    calculationMethod = ItcObservationDetails.CalculationMethod.SignalToNoise.SpectroscopyWithSNAt(
      sigma = 100,
      wavelengthAt = Wavelength.decimalNanometers.getOption(610).get,
      coadds = None,
      sourceFraction = 1.0,
      ditherOffset = Angle.Angle0
    ),
    analysisMethod = lsAnalysisMethod
  )

  val telescope = ItcTelescopeDetails(
    wfs = ItcWavefrontSensor.OIWFS
  )

  val gmosN = ItcInstrumentDetails(
    ObservingMode.SpectroscopyMode.GmosNorth(
      Wavelength.decimalNanometers.getOption(600).get,
      GmosNorthGrating.B1200_G5301,
      GmosNorthFpuParam(GmosNorthFpu.LongSlit_5_00),
      none,
      GmosCcdMode(
        GmosXBinning.One,
        GmosYBinning.One,
        GmosAmpCount.Twelve,
        GmosAmpGain.High,
        GmosAmpReadMode.Fast
      ).some,
      GmosRoi.FullFrame.some
    )
  )

  val conditions = ItcObservingConditions(
    ImageQuality.PointEight,
    CloudExtinction.OnePointFive,
    WaterVapor.Median,
    SkyBackground.Bright,
    1
  )

  def bodyCond(c: ItcObservingConditions) =
    ItcParameters(
      sourceDefinition,
      obs,
      c,
      telescope,
      gmosN
    )

  test("image quality".tag(LegacyITCTest)):
    Enumerated[ImageQuality].all.foreach: iq =>
      val result = localItc
        .calculateIntegrationTime(bodyCond(conditions.copy(iq = iq)).asJson.noSpaces)
      assert(result.fold(allowedErrors, _ => true))

  test("cloud extinction".tag(LegacyITCTest)):
    Enumerated[CloudExtinction].all.foreach: ce =>
      val result = localItc
        .calculateIntegrationTime(bodyCond(conditions.copy(cc = ce)).asJson.noSpaces)
      assert(result.fold(allowedErrors, _ => true))

  test("water vapor".tag(LegacyITCTest)):
    Enumerated[WaterVapor].all.foreach: wv =>
      val result = localItc
        .calculateIntegrationTime(bodyCond(conditions.copy(wv = wv)).asJson.noSpaces)
      assert(result.fold(allowedErrors, _ => true))

  test("sky background".tag(LegacyITCTest)):
    Enumerated[SkyBackground].all.foreach: sb =>
      val result = localItc
        .calculateIntegrationTime(bodyCond(conditions.copy(sb = sb)).asJson.noSpaces)
      assert(result.fold(allowedErrors, _ => true))

  val gnConf = ObservingMode.SpectroscopyMode.GmosNorth(
    Wavelength.decimalNanometers.getOption(600).get,
    GmosNorthGrating.B1200_G5301,
    GmosNorthFpuParam(GmosNorthFpu.LongSlit_1_00),
    none,
    GmosCcdMode(GmosXBinning.One,
                GmosYBinning.One,
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
      ItcInstrumentDetails(c)
    )

  test("gmos north grating".tag(LegacyITCTest)):
    Enumerated[GmosNorthGrating].all.foreach: d =>
      val result = localItc
        .calculateIntegrationTime(bodyConf(gnConf.copy(disperser = d)).asJson.noSpaces)
      assert(result.fold(allowedErrors, _ => true))

  test("gmos north filter".tag(LegacyITCTest)):
    Enumerated[GmosNorthFilter].all.foreach: f =>
      val result = localItc
        .calculateIntegrationTime(bodyConf(gnConf.copy(filter = f.some)).asJson.noSpaces)
      assert(result.fold(allowedErrors, _ => true))

  test("gmos north fpu".tag(LegacyITCTest)):
    Enumerated[GmosNorthFpu].all.foreach: f =>
      val result = localItc
        .calculateIntegrationTime(
          bodyConf(gnConf.copy(fpu = GmosNorthFpuParam(f)),
                   analysis = if (f.isIFU) ifuAnalysisMethod else lsAnalysisMethod
          ).asJson.noSpaces
        )
      assert(result.fold(allowedErrors, _ => true))

  val gsConf = ObservingMode.SpectroscopyMode.GmosSouth(
    Wavelength.decimalNanometers.getOption(600).get,
    GmosSouthGrating.B1200_G5321,
    GmosSouthFpuParam(GmosSouthFpu.LongSlit_1_00),
    none,
    GmosCcdMode(GmosXBinning.One,
                GmosYBinning.One,
                GmosAmpCount.Twelve,
                GmosAmpGain.High,
                GmosAmpReadMode.Fast
    ).some,
    GmosRoi.FullFrame.some
  )

  test("gmos south grating".tag(LegacyITCTest)):
    Enumerated[GmosSouthGrating].all.foreach: d =>
      val result = localItc
        .calculateIntegrationTime(bodyConf(gsConf.copy(disperser = d)).asJson.noSpaces)
      assert(result.fold(allowedErrors, _ => true))

  test("gmos south filter".tag(LegacyITCTest)):
    Enumerated[GmosSouthFilter].all.foreach: f =>
      val result = localItc
        .calculateIntegrationTime(bodyConf(gsConf.copy(filter = f.some)).asJson.noSpaces)
      assert(result.fold(allowedErrors, _ => true))

  test("gmos south fpu".tag(LegacyITCTest)):
    Enumerated[GmosSouthFpu].all.foreach: f =>
      val result = localItc
        .calculateIntegrationTime(
          bodyConf(gsConf.copy(fpu = GmosSouthFpuParam(f)),
                   analysis = if (f.isIFU) ifuAnalysisMethod else lsAnalysisMethod
          ).asJson.noSpaces
        )
      assert(result.fold(allowedErrors, _ => true))

  test("gmos south filter".tag(LegacyITCTest)):
    Enumerated[GmosSouthFilter].all.foreach: f =>
      val result = localItc
        .calculateIntegrationTime(bodyConf(gsConf.copy(filter = f.some)).asJson.noSpaces)
      assert(result.fold(allowedErrors, _ => true))

  def bodySED(c: UnnormalizedSED) =
    ItcParameters(
      sourceDefinition.copy(
        target = sourceDefinition.target.copy(
          sourceProfile = SourceProfile.unnormalizedSED
            .modifyOption(_ => c.some)(sourceDefinition.sourceProfile)
            .getOrElse(sourceDefinition.sourceProfile)
        )
      ),
      obs,
      conditions,
      telescope,
      gmosN
    )

  test("stellar library spectrum".tag(LegacyITCTest)):
    Enumerated[StellarLibrarySpectrum].all.foreach: f =>
      val result = localItc
        .calculateIntegrationTime(bodySED(UnnormalizedSED.StellarLibrary(f)).asJson.noSpaces)
      assert(result.fold(allowedErrors, _ => true))

  test("cool star".tag(LegacyITCTest)):
    Enumerated[CoolStarTemperature].all.foreach: f =>
      val result = localItc
        .calculateIntegrationTime(bodySED(UnnormalizedSED.CoolStarModel(f)).asJson.noSpaces)
      assert(result.fold(allowedErrors, _ => true))

  test("galaxy spectrum".tag(LegacyITCTest)):
    Enumerated[GalaxySpectrum].all.foreach: f =>
      val result = localItc
        .calculateIntegrationTime(bodySED(UnnormalizedSED.Galaxy(f)).asJson.noSpaces)
      assert(result.fold(allowedErrors, _ => true))

  test("planet spectrum".tag(LegacyITCTest)):
    Enumerated[PlanetSpectrum].all.foreach: f =>
      val result = localItc
        .calculateIntegrationTime(bodySED(UnnormalizedSED.Planet(f)).asJson.noSpaces)
      assert(result.fold(allowedErrors, _ => true))

  test("quasar spectrum".tag(LegacyITCTest)):
    Enumerated[QuasarSpectrum].all.foreach: f =>
      val result = localItc
        .calculateIntegrationTime(bodySED(UnnormalizedSED.Quasar(f)).asJson.noSpaces)
      assert(result.fold(allowedErrors, _ => true))

  test("hii region spectrum".tag(LegacyITCTest)):
    Enumerated[HIIRegionSpectrum].all.foreach: f =>
      val result = localItc
        .calculateIntegrationTime(bodySED(UnnormalizedSED.HIIRegion(f)).asJson.noSpaces)
      assert(result.fold(allowedErrors, _ => true))

  test("planetary nebula spectrum".tag(LegacyITCTest)):
    Enumerated[PlanetaryNebulaSpectrum].all.foreach: f =>
      val result = localItc
        .calculateIntegrationTime(bodySED(UnnormalizedSED.PlanetaryNebula(f)).asJson.noSpaces)
      assert(result.fold(allowedErrors, _ => true))

  def bodyIntMagUnits(c: BrightnessMeasure[Integrated]) =
    ItcParameters(
      sourceDefinition.copy(
        target = sourceDefinition.target.copy(
          sourceProfile = SourceProfile
            .integratedBrightnessIn(Band.R)
            .replace(c)(sourceDefinition.sourceProfile)
        )
      ),
      obs,
      conditions,
      telescope,
      gmosN
    )

  test("brightness integrated units".tag(LegacyITCTest)):
    Brightness.Integrated.all.toList.foreach: f =>
      val result = localItc
        .calculateIntegrationTime(
          bodyIntMagUnits(f.withValueTagged(BrightnessValue.unsafeFrom(5))).asJson.noSpaces
        )
      assert(result.fold(allowedErrors, _ => true))

  test("user defined SED".tag(LegacyITCTest)) {
    val userDefinedFluxDensities = NonEmptyMap.of(
      Wavelength.decimalNanometers.getOption(500).get -> PosBigDecimal.unsafeFrom(1.0),
      Wavelength.decimalNanometers.getOption(600).get -> PosBigDecimal.unsafeFrom(2.0),
      Wavelength.decimalNanometers.getOption(700).get -> PosBigDecimal.unsafeFrom(3.0)
    )

    val result = localItc
      .calculateIntegrationTime(
        bodySED(UnnormalizedSED.UserDefined(userDefinedFluxDensities)).asJson.noSpaces
      )

    assert(result.fold(allowedErrors, _ => true))
  }

  def bodySurfaceMagUnits(c: BrightnessMeasure[Surface]) =
    ItcParameters(
      sourceDefinition.copy(
        target = sourceDefinition.target.copy(
          sourceProfile = SourceProfile.Uniform(
            SpectralDefinition.BandNormalized(
              UnnormalizedSED.StellarLibrary(StellarLibrarySpectrum.A0V).some,
              SortedMap(Band.R -> c)
            )
          )
        )
      ),
      obs,
      conditions,
      telescope,
      gmosN
    )

  test("surface units".tag(LegacyITCTest)):
    Brightness.Surface.all.toList.foreach: f =>
      val result = localItc
        .calculateIntegrationTime(
          bodySurfaceMagUnits(f.withValueTagged(BrightnessValue.unsafeFrom(5))).asJson.noSpaces
        )
      assert(result.fold(allowedErrors, _ => true))

  def bodyIntGaussianMagUnits(c: BrightnessMeasure[Integrated]) =
    ItcParameters(
      sourceDefinition.copy(
        target = sourceDefinition.target.copy(
          sourceProfile = SourceProfile.Gaussian(
            Angle.fromDoubleArcseconds(10),
            SpectralDefinition.BandNormalized(
              UnnormalizedSED.StellarLibrary(StellarLibrarySpectrum.A0V).some,
              SortedMap(Band.R -> c)
            )
          )
        )
      ),
      obs,
      conditions,
      telescope,
      gmosN
    )

  test("gaussian units".tag(LegacyITCTest)):
    Brightness.Integrated.all.toList.foreach: f =>
      val result = localItc
        .calculateIntegrationTime(
          bodyIntGaussianMagUnits(f.withValueTagged(BrightnessValue.unsafeFrom(5))).asJson.noSpaces
        )
      assert(result.fold(allowedErrors, _ => true))

  def bodyPowerLaw(c: Int) =
    ItcParameters(
      sourceDefinition.copy(
        target = sourceDefinition.target.copy(
          sourceProfile = SourceProfile.Gaussian(
            Angle.fromDoubleArcseconds(10),
            SpectralDefinition.BandNormalized(
              UnnormalizedSED.PowerLaw(c).some,
              SortedMap(
                Band.R ->
                  BrightnessValue
                    .unsafeFrom(5)
                    .withUnit[VegaMagnitude]
                    .toMeasureTagged
              )
            )
          )
        )
      ),
      obs,
      conditions,
      telescope,
      gmosN
    )

  test("power law".tag(LegacyITCTest)):
    List(-10, 0, 10, 100).foreach: f =>
      val result = localItc
        .calculateIntegrationTime(bodyPowerLaw(f).asJson.noSpaces)
      assert(result.fold(allowedErrors, _ => true))

  def bodyBlackBody(c: PosInt) =
    ItcParameters(
      sourceDefinition.copy(
        target = sourceDefinition.target.copy(
          sourceProfile = SourceProfile.Gaussian(
            Angle.fromDoubleArcseconds(10),
            SpectralDefinition.BandNormalized(
              UnnormalizedSED.BlackBody(c.withUnit[Kelvin]).some,
              SortedMap(
                Band.R ->
                  BrightnessValue
                    .unsafeFrom(5)
                    .withUnit[VegaMagnitude]
                    .toMeasureTagged
              )
            )
          )
        )
      ),
      obs,
      conditions,
      telescope,
      gmosN
    )
