// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.legacy

import io.circe.*
import io.circe.generic.semiauto._
import io.circe.syntax.*
import lucuma.core.enums._
import lucuma.core.math.Angle
import lucuma.core.math.Redshift
import lucuma.core.math.Wavelength
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.UnnormalizedSED
import lucuma.itc.ItcObservingConditions
import lucuma.itc.search.ObservingMode.Spectroscopy._
import lucuma.itc.search.*
import lucuma.itc.search.syntax.all._
import lucuma.itc.search.syntax.sed._

import java.math.MathContext

// given Encoder[GmosNorth] = a =>
//   Json.obj(
//     ("instrument", Json.fromString(a.instrument.longName.toUpperCase.replace(" ", "_"))),
//     ("resolution", Json.fromInt(a.resolution.toInt)),
//     ("params", GmosNITCParams(a.disperser, a.fpu, a.filter).asJson),
//     ("wavelength", a.λ.asJson)
//   )
//
// given Encoder[GmosSouth] = a =>
//   Json.obj(
//     ("instrument", Json.fromString(a.instrument.longName.toUpperCase.replace(" ", "_"))),
//     ("resolution", Json.fromInt(a.resolution.toInt)),
//     ("params", GmosSITCParams(a.disperser, a.fpu, a.filter).asJson),
//     ("wavelength", a.λ.asJson)
//   )
//
// given Encoder[ObservingMode.Spectroscopy] = Encoder.instance {
//   case gn: GmosNorth => gn.asJson
//   case gs: GmosSouth => gs.asJson
// }
//
// given Encoder[Itc.Result] = Encoder.instance {
//   case f: Itc.Result.Success          =>
//     Json.obj(("resultType", Json.fromString("Success"))).deepMerge(f.asJson)
//   case Itc.Result.CalculationError(m) =>
//     Json.obj(("resultType", Json.fromString("Error")), ("msg", Json.fromString(m)))
//   case Itc.Result.SourceTooBright(m)  =>
//     Json.obj(("resultType", Json.fromString("Error")),
//              ("msg", Json.fromString(s"Source too bright $m"))
//     )
// }
//
private def toItcAirmass(m: Double): Double =
  if (m <= 1.35) 1.2 else if (m <= 1.75) 1.5 else 2.0

given Encoder[ItcObservingConditions] =
  import lucuma.itc.legacy.syntax.conditions.*
  Encoder.forProduct5("exactiq", "exactcc", "wv", "sb", "airmass") { a =>
    (Json.obj(
       "arcsec"     -> Json.fromBigDecimal(
         a.iq.toArcSeconds.value.toBigDecimal(MathContext.DECIMAL32)
       )
     ),
     Json.obj(
       "extinction" -> Json.fromBigDecimal(BigDecimal(a.cc.toBrightness))
     ),
     a.wv.ocs2Tag,
     a.sb.ocs2Tag,
     toItcAirmass(a.airmass)
    )
  }

private val encodeGmosNorthSpectroscopy: Encoder[ObservingMode.Spectroscopy.GmosNorth] =
  new Encoder[ObservingMode.Spectroscopy.GmosNorth] {
    def apply(a: ObservingMode.Spectroscopy.GmosNorth): Json =
      Json.obj(
        // Translate observing mode to OCS2 style
        "centralWavelength" -> Json.fromString(
          s"${Wavelength.decimalNanometers.reverseGet(a.λ)} nm"
        ),
        "filter"            -> Json.obj(
          "FilterNorth" -> a.filter.fold[Json](Json.fromString("NONE"))(r =>
            Json.fromString(r.ocs2Tag)
          )
        ),
        "grating"           -> Json.obj("DisperserNorth" -> Json.fromString(a.disperser.ocs2Tag)),
        "fpMask"            -> Json.obj("FPUnitNorth" -> Json.fromString(a.fpu.builtin.ocs2Tag)),
        // Remaining fields are defaulted for now.
        "spectralBinning"   -> Json.fromInt(1),
        "site"              -> Json.fromString("GN"),
        "ccdType"           -> Json.fromString("HAMAMATSU"),
        "ampReadMode"       -> Json.fromString("SLOW"),
        "builtinROI"        -> Json.fromString("FULL_FRAME"),
        "spatialBinning"    -> Json.fromInt(1),
        "customSlitWidth"   -> Json.Null,
        "ampGain"           -> Json.fromString("LOW")
      )
  }

private val encodeGmosSouthSpectroscopy: Encoder[ObservingMode.Spectroscopy.GmosSouth] =
  new Encoder[ObservingMode.Spectroscopy.GmosSouth] {
    def apply(a: ObservingMode.Spectroscopy.GmosSouth): Json =
      Json.obj(
        // Translate observing mode to OCS2 style
        "centralWavelength" -> Json.fromString(
          s"${Wavelength.decimalNanometers.reverseGet(a.λ)} nm"
        ),
        "filter"            -> Json.obj(
          "FilterSouth" -> a.filter.fold[Json](Json.fromString("NONE"))(r =>
            Json.fromString(r.ocs2Tag)
          )
        ),
        "grating"           -> Json.obj("DisperserSouth" -> Json.fromString(a.disperser.ocs2Tag)),
        "fpMask"            -> Json.obj("FPUnitSouth" -> Json.fromString(a.fpu.builtin.ocs2Tag)),
        // Remaining fields are defaulted for now.
        "spectralBinning"   -> Json.fromInt(1),
        "site"              -> Json.fromString("GS"),
        "ccdType"           -> Json.fromString("HAMAMATSU"),
        "ampReadMode"       -> Json.fromString("SLOW"),
        "builtinROI"        -> Json.fromString("FULL_FRAME"),
        "spatialBinning"    -> Json.fromInt(1),
        "customSlitWidth"   -> Json.Null,
        "ampGain"           -> Json.fromString("LOW")
      )
  }

private given Encoder[ItcInstrumentDetails] =
  new Encoder[ItcInstrumentDetails] {
    def apply(a: ItcInstrumentDetails): Json =
      a.mode match {
        case a: ObservingMode.Spectroscopy.GmosNorth =>
          Json.obj("GmosParameters" -> encodeGmosNorthSpectroscopy(a))
        case a: ObservingMode.Spectroscopy.GmosSouth =>
          Json.obj("GmosParameters" -> encodeGmosSouthSpectroscopy(a))
      }
  }

private given Encoder[ItcWavefrontSensor] = Encoder[String].contramap(_.ocs2Tag)

private given Encoder[ItcTelescopeDetails] =
  new Encoder[ItcTelescopeDetails] {
    def apply(a: ItcTelescopeDetails): Json =
      Json.obj(
        "mirrorCoating"  -> Json.fromString("SILVER"),
        "instrumentPort" -> Json.fromString("SIDE_LOOKING"),
        "wfs"            -> a.wfs.asJson
      )
  }

private given Encoder[SourceProfile] =
  new Encoder[SourceProfile] {
    import SourceProfile._
    def apply(a: SourceProfile): Json =
      a match {
        case _: Point    =>
          Json.obj("PointSource" -> Json.obj())
        case _: Uniform  => Json.obj("UniformSource" -> Json.obj())
        case g: Gaussian =>
          Json.obj(
            "GaussianSource" -> Json.obj(
              "fwhm" -> Angle.signedDecimalArcseconds.get(g.fwhm).asJson
            )
          )
      }
  }

private given Encoder[UnnormalizedSED] =
  new Encoder[UnnormalizedSED] {
    import UnnormalizedSED._
    def apply(a: UnnormalizedSED): Json =
      a match {
        case BlackBody(t)       =>
          Json.obj(
            "BlackBody" -> Json.obj(
              "temperature" -> Json.fromDoubleOrNull(t.value.value.toDouble)
            )
          )
        case PowerLaw(i)        =>
          Json.obj("PowerLaw" -> Json.obj("index" -> Json.fromDoubleOrNull(i.toDouble)))
        case StellarLibrary(s)  =>
          Json.obj("Library" -> Json.obj("LibraryStar" -> Json.fromString(s.ocs2Tag)))
        case s: CoolStarModel   =>
          Json.obj("Library" -> Json.obj("LibraryStar" -> Json.fromString(s.ocs2Tag)))
        case PlanetaryNebula(s) =>
          Json.obj("Library" -> Json.obj("LibraryStar" -> Json.fromString(s.ocs2Tag)))
        case Galaxy(s)          =>
          Json.obj("Library" -> Json.obj("LibraryNonStar" -> Json.fromString(s.ocs2Tag)))
        case Planet(s)          =>
          Json.obj("Library" -> Json.obj("LibraryNonStar" -> Json.fromString(s.ocs2Tag)))
        case HIIRegion(s)       =>
          Json.obj("Library" -> Json.obj("LibraryNonStar" -> Json.fromString(s.ocs2Tag)))
        case Quasar(s)          =>
          Json.obj("Library" -> Json.obj("LibraryNonStar" -> Json.fromString(s.ocs2Tag)))
        case _                  => // TODO UserDefined
          Json.obj("Library" -> Json.Null)
      }
  }

private given Encoder[Band] =
  Encoder[String].contramap(_.shortName)

private given Encoder[Redshift] =
  Encoder.forProduct1("z")(_.z)

given Encoder[ItcSourceDefinition] =
  new Encoder[ItcSourceDefinition] {
    def apply(s: ItcSourceDefinition): Json = {
      val source = s.profile match {
        case _: SourceProfile.Point    =>
          Json.obj("PointSource" -> Json.obj())
        case _: SourceProfile.Uniform  => Json.obj("UniformSource" -> Json.obj())
        case g: SourceProfile.Gaussian =>
          Json.obj(
            "GaussianSource" -> Json.obj(
              "fwhm" -> Angle.signedDecimalArcseconds.get(g.fwhm).asJson
            )
          )
      }

      val units: Json = s.profile match {
        case SourceProfile.Point(SpectralDefinition.BandNormalized(_, brightnesses))
            if brightnesses.contains(s.normBand) =>
          brightnesses.get(s.normBand).map(_.units.serialized) match {
            case Some("VEGA_MAGNITUDE")                  => Json.obj("MagnitudeSystem" -> Json.fromString("Vega"))
            case Some("AB_MAGNITUDE")                    => Json.obj("MagnitudeSystem" -> Json.fromString("AB"))
            case Some("JANSKY")                          => Json.obj("MagnitudeSystem" -> Json.fromString("Jy"))
            case Some("W_PER_M_SQUARED_PER_UM")          =>
              Json.obj("MagnitudeSystem" -> Json.fromString("W/m²/µm"))
            case Some("ERG_PER_S_PER_CM_SQUARED_PER_A")  =>
              Json.obj("MagnitudeSystem" -> Json.fromString("erg/s/cm²/Å"))
            case Some("ERG_PER_S_PER_CM_SQUARED_PER_HZ") =>
              Json.obj("MagnitudeSystem" -> Json.fromString("erg/s/cm²/Hz"))
            case _                                       =>
              Json.Null
          }
        // FIXME Support emision lines
        // case SourceProfile.Point(SpectralDefinition.EmissionLines(_, brightnesses)) =>
        //   Json.Null
        //     if brightnesses.contains(s.normBand) =>
        //   brightnesses.get(s.normBand).map(_.units.serialized) match {
        //     case Some("VEGA_MAGNITUDE")                  => Json.obj("MagnitudeSystem" -> Json.fromString("Vega"))
        //     case Some("AB_MAGNITUDE")                    => Json.obj("MagnitudeSystem" -> Json.fromString("AB"))
        //     case Some("JANSKY")                          => Json.obj("MagnitudeSystem" -> Json.fromString("Jy"))
        //     case Some("W_PER_M_SQUARED_PER_UM")          =>
        //       Json.obj("MagnitudeSystem" -> Json.fromString("W/m²/µm"))
        //     case Some("ERG_PER_S_PER_CM_SQUARED_PER_A")  =>
        //       Json.obj("MagnitudeSystem" -> Json.fromString("erg/s/cm²/Å"))
        //     case Some("ERG_PER_S_PER_CM_SQUARED_PER_HZ") =>
        //       Json.obj("MagnitudeSystem" -> Json.fromString("erg/s/cm²/Hz"))
        //     case _                                       =>
        //       Json.Null
        //   }
        case SourceProfile.Uniform(SpectralDefinition.BandNormalized(_, brightnesses))
            if brightnesses.contains(s.normBand) =>
          brightnesses.get(s.normBand).map(_.units.serialized) match {
            case Some("VEGA_MAG_PER_ARCSEC_SQUARED")                        =>
              Json.obj("SurfaceBrightness" -> Json.fromString("Vega mag/arcsec²"))
            case Some("AB_MAG_PER_ARCSEC_SQUARED")                          =>
              Json.obj("SurfaceBrightness" -> Json.fromString("AB mag/arcsec²"))
            case Some("JY_PER_ARCSEC_SQUARED")                              =>
              Json.obj("SurfaceBrightness" -> Json.fromString("Jy/arcsec²"))
            case Some("W_PER_M_SQUARED_PER_UM_PER_ARCSEC_SQUARED")          =>
              Json.obj("SurfaceBrightness" -> Json.fromString("W/m²/µm/arcsec²"))
            case Some("ERG_PER_S_PER_CM_SQUARED_PER_A_PER_ARCSEC_SQUARED")  =>
              Json.obj("SurfaceBrightness" -> Json.fromString("erg/s/cm²/Å/arcsec²"))
            case Some("ERG_PER_S_PER_CM_SQUARED_PER_HZ_PER_ARCSEC_SQUARED") =>
              Json.obj("SurfaceBrightness" -> Json.fromString("erg/s/cm²/Hz/arcsec²"))
            case _                                                          =>
              Json.Null
          }
        case SourceProfile.Gaussian(_, SpectralDefinition.BandNormalized(_, brightnesses))
            if brightnesses.contains(s.normBand) =>
          brightnesses.get(s.normBand).map(_.units.serialized) match {
            case Some("VEGA_MAGNITUDE")                  => Json.obj("MagnitudeSystem" -> Json.fromString("Vega"))
            case Some("AB_MAGNITUDE")                    => Json.obj("MagnitudeSystem" -> Json.fromString("AB"))
            case Some("JANSKY")                          => Json.obj("MagnitudeSystem" -> Json.fromString("Jy"))
            case Some("W_PER_M_SQUARED_PER_UM")          =>
              Json.obj("MagnitudeSystem" -> Json.fromString("W/m²/µm"))
            case Some("ERG_PER_S_PER_CM_SQUARED_PER_A")  =>
              Json.obj("MagnitudeSystem" -> Json.fromString("erg/s/cm²/Å"))
            case Some("ERG_PER_S_PER_CM_SQUARED_PER_HZ") =>
              Json.obj("MagnitudeSystem" -> Json.fromString("erg/s/cm²/Hz"))
            case _                                       =>
              Json.Null
          }

        // FIXME Support emision lines
        case _ => Json.Null
      }

      val value: Json = s.profile match {
        case SourceProfile.Point(SpectralDefinition.BandNormalized(_, brightnesses))
            if brightnesses.contains(s.normBand) =>
          brightnesses
            .get(s.normBand)
            .map(_.value.toDouble)
            .flatMap(Json.fromDouble)
            .getOrElse(Json.Null)
        case SourceProfile.Uniform(SpectralDefinition.BandNormalized(_, brightnesses))
            if brightnesses.contains(s.normBand) =>
          brightnesses
            .get(s.normBand)
            .map(_.value.toDouble)
            .flatMap(Json.fromDouble)
            .getOrElse(Json.Null)
        case SourceProfile.Gaussian(_, SpectralDefinition.BandNormalized(_, brightnesses))
            if brightnesses.contains(s.normBand) =>
          brightnesses
            .get(s.normBand)
            .map(_.value.toDouble)
            .flatMap(Json.fromDouble)
            .getOrElse(Json.Null)
        // FIXME: Handle emission line
        case _ => Json.Null
      }

      val distribution = s.profile match {
        case SourceProfile.Point(SpectralDefinition.BandNormalized(sed, _))       =>
          sed.asJson
        // FIXME support emmision lines
        // case SourceProfile.Point(SpectralDefinition.EmissionLines(sed, _))        =>
        //   Json.Null
        case SourceProfile.Uniform(SpectralDefinition.BandNormalized(sed, _))     =>
          sed.asJson
        case SourceProfile.Gaussian(_, SpectralDefinition.BandNormalized(sed, _)) =>
          sed.asJson
        // FIXME: Handle emission line
        case _                                                                    => Json.Null
      }

      Json.obj("profile"      -> source,
               "normBand"     -> s.normBand.asJson,
               "norm"         -> value,
               "redshift"     -> s.redshift.asJson,
               "units"        -> units,
               "distribution" -> distribution
      )
    }
  }

given Encoder[ItcParameters] =
  deriveEncoder[ItcParameters]
