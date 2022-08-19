// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import algebra.instances.all.given
import buildinfo.BuildInfo
import cats._
import cats.data._
import cats.effect._
import cats.syntax.all._
import coulomb.*
import coulomb.ops.algebra.spire.all.given
import coulomb.policy.spire.standard.given
import coulomb.syntax.*
import coulomb.units.si.*
import coulomb.units.si.given
import coulomb.units.si.prefixes.*
import coulomb.units.time.*
import edu.gemini.grackle._
import edu.gemini.grackle.circe.CirceMapping
import eu.timepit.refined._
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.numeric.PosLong
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Encoder
import io.circe.Json
import lucuma.core.enums._
import lucuma.core.math.Angle
import lucuma.core.math.BrightnessUnits._
import lucuma.core.math.RadialVelocity
import lucuma.core.math.Wavelength
import lucuma.core.math.dimensional.Units._
import lucuma.core.math.dimensional._
import lucuma.core.math.units._
import lucuma.core.model.NonNegDuration
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.UnnormalizedSED
import lucuma.core.model.UnnormalizedSED._
import lucuma.core.syntax.enumerated._
import lucuma.core.syntax.string._
import lucuma.core.util.Enumerated
import lucuma.itc.GmosNITCParams
import lucuma.itc.GmosSITCParams
import lucuma.itc.Itc
import lucuma.itc.ItcObservingConditions
import lucuma.itc.SignificantFigures
import lucuma.itc.SpectroscopyParams
import lucuma.itc.UpstreamException
import lucuma.itc.given
import lucuma.itc.search.GmosNorthFpuParam
import lucuma.itc.search.GmosSouthFpuParam
import lucuma.itc.search.ObservingMode
import lucuma.itc.search.ObservingMode.Spectroscopy.GmosNorth
import lucuma.itc.search.ObservingMode.Spectroscopy.GmosSouth
import lucuma.itc.search.Result.Spectroscopy
import lucuma.itc.search.SpectroscopyResults
import lucuma.itc.search.TargetProfile
import lucuma.itc.service.config._
import lucuma.itc.service.syntax.all._
import natchez.Trace
import org.typelevel.log4cats.Logger

import java.math.RoundingMode
import java.time.Duration
import java.time.Instant
import java.time.ZoneId
import java.time.ZoneOffset
import java.time.format.DateTimeFormatter
import scala.collection.immutable.SortedMap
import scala.concurrent.duration._
import scala.io.Source
import scala.util.Using

import Query._
import Value._
import QueryCompiler._

trait GrackleParsers:
  def bigDecimalValue(v: Value): Option[BigDecimal] =
    v match
      case IntValue(r)    => BigDecimal(r).some
      case FloatValue(r)  => BigDecimal(r).some
      case StringValue(r) => Either.catchNonFatal(BigDecimal(r)).toOption
      case _              => none

  def parseFwhw(units: List[(String, Value)]): Option[Angle] =
    units.find(_._2 != Value.AbsentValue) match
      case Some(("microarcseconds", n)) =>
        bigDecimalValue(n).map(n => Angle.microarcseconds.reverseGet(n.toLong))
      case Some(("milliarcseconds", n)) =>
        bigDecimalValue(n).map(n => Angle.milliarcseconds.reverseGet(n.toInt))
      case Some(("arcseconds", n))      =>
        bigDecimalValue(n).map(n => Angle.arcseconds.reverseGet(n.toInt))
      case _                            => None

  def parseNonNegDuration(units: List[(String, Value)]): Option[NonNegDuration] =
    (units.find(_._2 != Value.AbsentValue) match
      case Some(("microseconds", IntValue(n))) =>
        Duration.ofNanos(n * 1000).some
      case Some(("milliseconds", n))           =>
        bigDecimalValue(n).map(n => Duration.ofNanos((n * 1e6).toLong))
      case Some(("seconds", n))                =>
        bigDecimalValue(n).map(n => Duration.ofNanos((n * 1e9).toLong))
      case Some(("minutes", n))                =>
        bigDecimalValue(n).map(n => Duration.ofNanos(((n * 60) * 1e9).toLong))
      case Some(("hours", n))                  =>
        bigDecimalValue(n).map(n => Duration.ofNanos(((n * 60 * 60) * 1e9).toLong))
      case _                                   => None
    ).flatMap(NonNegDuration.from(_).toOption)

  def parseWavelength(units: List[(String, Value)]): Option[Wavelength] =
    units.find(_._2 != Value.AbsentValue) match
      case Some(("picometers", IntValue(n))) =>
        Wavelength.fromPicometers.getOption(n)
      case Some(("angstroms", n))            =>
        bigDecimalValue(n).flatMap(Wavelength.decimalAngstroms.getOption)
      case Some(("nanometers", n))           =>
        bigDecimalValue(n).flatMap(Wavelength.decimalNanometers.getOption)
      case Some(("micrometers", n))          =>
        bigDecimalValue(n).flatMap(Wavelength.decimalMicrometers.getOption)
      case _                                 => None

  def parseRadialVelocity(units: List[(String, Value)]): Option[RadialVelocity] =
    units.find(_._2 != Value.AbsentValue) match
      case Some(("centimetersPerSecond", IntValue(n))) =>
        RadialVelocity(n.withUnit[CentimetersPerSecond].toValue[BigDecimal].toUnit[MetersPerSecond])
      case Some(("metersPerSecond", n))                =>
        bigDecimalValue(n).flatMap(v => RadialVelocity(v.withUnit[MetersPerSecond]))
      case Some(("kilometersPerSecond", n))            =>
        bigDecimalValue(n).flatMap(v => RadialVelocity.kilometerspersecond.getOption(v))
      case _                                           => None

trait GracklePartials extends GrackleParsers:
  type Partial = IorNec[Problem, Environment]

  def enumTags[A: Enumerated] =
    Enumerated[A].all.fproductLeft(_.tag.toScreamingSnakeCase).toMap

  val bandItems            = enumTags[Band]
  val stellarLibraryItems  = enumTags[StellarLibrarySpectrum]
  val coolStarItems        = enumTags[CoolStarTemperature]
  val galaxyItems          = enumTags[GalaxySpectrum]
  val planetItems          = enumTags[PlanetSpectrum]
  val hiiRegionItems       = enumTags[HIIRegionSpectrum]
  val planetaryNebulaItems = enumTags[PlanetaryNebulaSpectrum]
  val quasarItems          = enumTags[QuasarSpectrum]

  val integratedUnitsItems = enumTags[Units Of Brightness[Integrated]]
  val surfaceUnitsItems    = enumTags[Units Of Brightness[Surface]]

  val gnFilter  = enumTags[GmosNorthFilter]
  val gnGrating = enumTags[GmosNorthGrating]
  val gnFpu     = enumTags[GmosNorthFpu]

  val gsFilter  = enumTags[GmosSouthFilter]
  val gsGrating = enumTags[GmosSouthGrating]
  val gsFpu     = enumTags[GmosSouthFpu]

  val iqItems = enumTags[ImageQuality]
  val ceItems = enumTags[CloudExtinction]
  val wvItems = enumTags[WaterVapor]
  val sbItems = enumTags[SkyBackground]

  val AirMassBuckets = Vector(BigDecimal(1.2), BigDecimal(1.5), BigDecimal(2.0))

  def wavelengthPartial: PartialFunction[(Partial, (String, Value)), Partial] =
    case (i, ("wavelength", ObjectValue(units)))
        if units.filter(_._2 != Value.AbsentValue).length != 1 =>
      val presentUnits =
        units.filter(_._2 != Value.AbsentValue).map(_._1).mkString("{", ", ", "}")
      i.addProblem(s"Wavelength defined with multiple units $presentUnits")
    case (i, ("wavelength", ObjectValue(units))) =>
      val wavelength: Option[Wavelength] = parseWavelength(units)
      wavelength
        .map(w => cursorEnvAdd("wavelength", w)(i))
        .getOrElse(i.addProblem("Wavelength couldn't be parsed"))

  def radialVelocityPartial: PartialFunction[(Partial, (String, Value)), Partial] =
    // radialVelocity
    case (i, ("radialVelocity", ObjectValue(r))) =>
      parseRadialVelocity(r) match
        case Some(r) =>
          cursorEnvAdd("radialVelocity", r)(i)
        case _       =>
          i.addProblem(s"Radial Velocity value is not valid $r")

  def signalToNoisePartial: PartialFunction[(Partial, (String, Value)), Partial] =
    // signalToNoise
    case (i, ("signalToNoise", r)) =>
      bigDecimalValue(r) match
        case Some(r) if r > 0 =>
          refineV[Positive](r)
            .fold(i.addProblem, v => cursorEnvAdd("signalToNoise", v)(i))
        case Some(r)          =>
          i.addProblem(s"signalToNoise value $r must be positive")
        case _                =>
          i.addProblem(s"Not valid signalToNoise value $r")

  def brightnessesReader[A](
    br:         List[Value],
    unitsItems: Map[String, Units Of Brightness[A]]
  ): IorNec[String, SortedMap[Band, BrightnessMeasure[A]]] = br
    .map {
      case ObjectValue(
            List(("band", TypedEnumValue(EnumValue(b, _, _, _))),
                 ("value", v),
                 ("units", TypedEnumValue(EnumValue(u, _, _, _))),
                 ("error", _)
            )
          ) =>
        val band  = bandItems.get(b)
        val value = bigDecimalValue(v)
        val units = unitsItems.get(u)
        (band, value, units)
          .mapN { (b, v, u) =>
            b -> u.withValueTagged(v)
          }
          .toRightIorNec("Invalid brightness")
      case e => s"Invalid brighness entry $e".leftIorNec
    }
    .sequence
    .map(v => SortedMap(v: _*))

  def sedReader(sed: List[(String, Value)]): IorNec[String, UnnormalizedSED] =
    sed match
      case ("stellarLibrary", TypedEnumValue(EnumValue(s, _, _, _))) ::
          ("coolStar", AbsentValue) ::
          ("galaxy", AbsentValue) ::
          ("planet", AbsentValue) ::
          ("quasar", AbsentValue) ::
          ("hiiRegion", AbsentValue) ::
          ("planetaryNebula", AbsentValue) ::
          ("powerLaw", AbsentValue) ::
          ("blackBodyTempK", AbsentValue) ::
          ("fluxDensities", AbsentValue) :: Nil =>
        stellarLibraryItems
          .get(s)
          .map(StellarLibrary(_))
          .toRightIorNec(s"Invalid stellar library value $s")

      case ("stellarLibrary", AbsentValue) ::
          ("coolStar", TypedEnumValue(EnumValue(s, _, _, _))) ::
          ("galaxy", AbsentValue) ::
          ("planet", AbsentValue) ::
          ("quasar", AbsentValue) ::
          ("hiiRegion", AbsentValue) ::
          ("planetaryNebula", AbsentValue) ::
          ("powerLaw", AbsentValue) ::
          ("blackBodyTempK", AbsentValue) ::
          ("fluxDensities", AbsentValue) :: Nil =>
        coolStarItems
          .get(s)
          .map(CoolStarModel(_))
          .toRightIorNec(s"Invalid coolstar value $s")

      case ("stellarLibrary", AbsentValue) ::
          ("coolStar", AbsentValue) ::
          ("galaxy", TypedEnumValue(EnumValue(s, _, _, _))) ::
          ("planet", AbsentValue) ::
          ("quasar", AbsentValue) ::
          ("hiiRegion", AbsentValue) ::
          ("planetaryNebula", AbsentValue) ::
          ("powerLaw", AbsentValue) ::
          ("blackBodyTempK", AbsentValue) ::
          ("fluxDensities", AbsentValue) :: Nil =>
        galaxyItems
          .get(s)
          .map(Galaxy(_))
          .toRightIorNec(s"Invalid galaxy value $s")

      case ("stellarLibrary", AbsentValue) ::
          ("coolStar", AbsentValue) ::
          ("galaxy", AbsentValue) ::
          ("planet", TypedEnumValue(EnumValue(s, _, _, _))) ::
          ("quasar", AbsentValue) ::
          ("hiiRegion", AbsentValue) ::
          ("planetaryNebula", AbsentValue) ::
          ("powerLaw", AbsentValue) ::
          ("blackBodyTempK", AbsentValue) ::
          ("fluxDensities", AbsentValue) :: Nil =>
        planetItems
          .get(s)
          .map(Planet(_))
          .toRightIorNec(s"Invalid planet value $s")

      case ("stellarLibrary", AbsentValue) ::
          ("coolStar", AbsentValue) ::
          ("galaxy", AbsentValue) ::
          ("planet", AbsentValue) ::
          ("quasar", TypedEnumValue(EnumValue(s, _, _, _))) ::
          ("hiiRegion", AbsentValue) ::
          ("planetaryNebula", AbsentValue) ::
          ("powerLaw", AbsentValue) ::
          ("blackBodyTempK", AbsentValue) ::
          ("fluxDensities", AbsentValue) :: Nil =>
        quasarItems
          .get(s)
          .map(Quasar(_))
          .toRightIorNec(s"Invalid quasar value $s")

      case ("stellarLibrary", AbsentValue) ::
          ("coolStar", AbsentValue) ::
          ("galaxy", AbsentValue) ::
          ("planet", AbsentValue) ::
          ("quasar", AbsentValue) ::
          ("hiiRegion", TypedEnumValue(EnumValue(s, _, _, _))) ::
          ("planetaryNebula", AbsentValue) ::
          ("powerLaw", AbsentValue) ::
          ("blackBodyTempK", AbsentValue) ::
          ("fluxDensities", AbsentValue) :: Nil =>
        hiiRegionItems
          .get(s)
          .map(HIIRegion(_))
          .toRightIorNec(s"Invalid hii region value $s")

      case ("stellarLibrary", AbsentValue) ::
          ("coolStar", AbsentValue) ::
          ("galaxy", AbsentValue) ::
          ("planet", AbsentValue) ::
          ("quasar", AbsentValue) ::
          ("hiiRegion", AbsentValue) ::
          ("planetaryNebula", TypedEnumValue(EnumValue(s, _, _, _))) ::
          ("powerLaw", AbsentValue) ::
          ("blackBodyTempK", AbsentValue) ::
          ("fluxDensities", AbsentValue) :: Nil =>
        planetaryNebulaItems
          .get(s)
          .map(PlanetaryNebula(_))
          .toRightIorNec(s"Invalid planetary nebula value $s")

      case ("stellarLibrary", AbsentValue) ::
          ("coolStar", AbsentValue) ::
          ("galaxy", AbsentValue) ::
          ("planet", AbsentValue) ::
          ("quasar", AbsentValue) ::
          ("hiiRegion", AbsentValue) ::
          ("planetaryNebula", AbsentValue) ::
          ("powerLaw", r) ::
          ("blackBodyTempK", AbsentValue) ::
          ("fluxDensities", AbsentValue) :: Nil =>
        bigDecimalValue(r) match
          case Some(r) =>
            val powerLaw = UnnormalizedSED.PowerLaw(r)
            powerLaw.rightIorNec
          case _       =>
            s"Not a valid power law value $r".leftIorNec

      case ("stellarLibrary", AbsentValue) ::
          ("coolStar", AbsentValue) ::
          ("galaxy", AbsentValue) ::
          ("planet", AbsentValue) ::
          ("quasar", AbsentValue) ::
          ("hiiRegion", AbsentValue) ::
          ("planetaryNebula", AbsentValue) ::
          ("powerLaw", AbsentValue) ::
          ("blackBodyTempK", r) ::
          ("fluxDensities", AbsentValue) :: Nil =>
        bigDecimalValue(r) match
          case Some(r) if r > 0 =>
            val blackBody = refineV[Positive](r.toInt)
              .map(k => UnnormalizedSED.BlackBody(k.withUnit[Kelvin]))
              .toOption
            blackBody.toRightIorNec(s"Not a valid black body value $r")
          case Some(r)          =>
            s"black body value $r must be positive".leftIorNec
          case _                =>
            s"Not a valid black body value $r".leftIorNec

      case ("stellarLibrary", AbsentValue) ::
          ("coolStar", AbsentValue) ::
          ("galaxy", AbsentValue) ::
          ("planet", AbsentValue) ::
          ("quasar", AbsentValue) ::
          ("hiiRegion", AbsentValue) ::
          ("planetaryNebula", AbsentValue) ::
          ("powerLaw", AbsentValue) ::
          ("blackBodyTempK", AbsentValue) ::
          ("fluxDensities", fd) :: Nil =>
        s"Flux densities not yet supported $fd".leftIorNec

      case _ =>
        s"Error on spectral definition parameter".leftIorNec

  def exposuresPartial: PartialFunction[(Partial, (String, Value)), Partial] =
    case (i, ("exposures", v)) =>
      posLongValue(v)
        .map(m => cursorEnvAdd("exposures", m)(i))
        .getOrElse(i.addProblem("Exposures must be a positive int"))

  def exposureTimePartial: PartialFunction[(Partial, (String, Value)), Partial] =
    case (i, ("exposureTime", ObjectValue(units)))
        if units.filter(_._2 != Value.AbsentValue).length != 1 =>
      val presentUnits =
        units.filter(_._2 != Value.AbsentValue).map(_._1).mkString("{", ", ", "}")
      i.addProblem(s"Exposure time defined with multiple units $presentUnits")

    case (i, ("exposureTime", ObjectValue(v))) =>
      parseNonNegDuration(v)
        .map(m => cursorEnvAdd("exposureTime", m)(i))
        .getOrElse(i.addProblem("Exposure time must be a valid duration"))

  def sourceProfilePartial: PartialFunction[(Partial, (String, Value)), Partial] =
    case (i, ("sourceProfile", ObjectValue(v))) if v.length === 3 =>
      v match {
        case ("point", AbsentValue) ::
            ("uniform", ObjectValue(ov)) ::
            ("gaussian", AbsentValue) :: Nil =>
          val uniform: IorNec[String, SourceProfile] = ov match
            case ("bandNormalized", ObjectValue(bn)) ::
                ("emissionLines", AbsentValue) ::
                Nil =>
              bn match
                case ("sed", ObjectValue(sed)) ::
                    ("brightnesses", ListValue(br)) ::
                    ("editBrightnesses", AbsentValue) ::
                    ("deleteBrightnesses", AbsentValue) :: Nil =>
                  val brightesses = brightnessesReader(br, surfaceUnitsItems)
                  val sedResult   = sedReader(sed)
                  (sedResult, brightesses).mapN((s, b) =>
                    SourceProfile.Uniform(
                      SpectralDefinition.BandNormalized(s, b)
                    )
                  )
                case _ => "Error parsing uniform profile".leftIorNec

            case _ => "Emission lines are not supported yet".leftIorNec

          uniform.map(p => cursorEnvAdd("sourceProfile", p)(i)).leftProblems.flatten
        case ("point", ObjectValue(ov)) ::
            ("uniform", AbsentValue) ::
            ("gaussian", AbsentValue) :: Nil =>
          val point: IorNec[String, SourceProfile] = ov match
            case ("bandNormalized", ObjectValue(bn)) ::
                ("emissionLines", AbsentValue) ::
                Nil =>
              bn match
                case ("sed", ObjectValue(sed)) ::
                    ("brightnesses", ListValue(br)) ::
                    ("editBrightnesses", AbsentValue) ::
                    ("deleteBrightnesses", AbsentValue) :: Nil =>
                  val brightesses = brightnessesReader(br, integratedUnitsItems)
                  val sedResult   = sedReader(sed)

                  (sedResult, brightesses).mapN((s, b) =>
                    SourceProfile.Point(
                      SpectralDefinition.BandNormalized(s, b)
                    )
                  )
                case _ => "Error parsing point profile".leftIorNec

            case _ => "Emission lines are not supported yet".leftIorNec

          point.map(p => cursorEnvAdd("sourceProfile", p)(i)).leftProblems.flatten

        case ("point", AbsentValue) ::
            ("uniform", AbsentValue) ::
            ("gaussian", ObjectValue(ov)) :: Nil =>
          val gaussian: IorNec[String, SourceProfile] = ov match
            case ("fwhm", ObjectValue(fw)) ::
                ("spectralDefinition", ObjectValue(sd)) ::
                Nil =>
              val fwhmResult = parseFwhw(fw).toRightIorNec(s"Cannot parse fwhm $fw")

              sd match
                case ("bandNormalized", ObjectValue(bn)) ::
                    ("emissionLines", AbsentValue) ::
                    Nil =>
                  bn match
                    case ("sed", ObjectValue(sed)) ::
                        ("brightnesses", ListValue(br)) ::
                        ("editBrightnesses", AbsentValue) ::
                        ("deleteBrightnesses", AbsentValue) :: Nil =>
                      val brightesses = brightnessesReader(br, integratedUnitsItems)
                      val sedResult   = sedReader(sed)

                      (fwhmResult, sedResult, brightesses).mapN((f, s, b) =>
                        SourceProfile.Gaussian(
                          f,
                          SpectralDefinition.BandNormalized(s, b)
                        )
                      )
                    case _ => "Error parsing band normalized values".leftIorNec

                case _ => "Emission lines are not supported yet".leftIorNec

            case _ => "Cannot parse gaussian profile input".leftIorNec

          gaussian.map(p => cursorEnvAdd("sourceProfile", p)(i)).leftProblems.flatten
        case _ => i.addProblem("Unsupported source profile")
      }
    case (i, ("sourceProfile", v))                                =>
      i.addProblem(s"Cannot parse sourceProfile $v")

  def bandPartial: PartialFunction[(Partial, (String, Value)), Partial] =
    case (i, ("band", TypedEnumValue(EnumValue(b, _, _, _)))) =>
      bandItems
        .get(b)
        .map(b => cursorEnvAdd("band", b)(i))
        .getOrElse(i.addProblem("Cannot parse band"))

  def posIntValue(v: Value): Option[PosInt] =
    v match
      case IntValue(r) if r > 0 =>
        refineV[Positive](r).fold(_ => none, _.some)
      case _                    => none

  def posLongValue(v: Value): Option[PosLong] =
    v match
      case IntValue(r) if r > 0 =>
        refineV[Positive](r.toLong).fold(_ => none, _.some)
      case _                    => none

  def significantFiguresPartial: PartialFunction[(Partial, (String, Value)), Partial] =

    case (i, ("significantFigures", ObjectValue(r))) =>
      r match
        case ("xAxis", x) :: ("yAxis", y) :: Nil =>
          (posIntValue(x), posIntValue(y)) match
            case (None, None) =>
              i.addProblem(s"Not valid significantFigures value $r")
            case (x, y)       =>
              cursorEnvAdd("significantFigures", SignificantFigures(x, y))(i)

        case _ =>
          i.addProblem(s"Not valid significantFigures value $r")

  def instrumentModesPartial: PartialFunction[(Partial, (String, Value)), Partial] =
    case (i, ("modes", ListValue(m))) =>
      val modes = m.collect {
        case ObjectValue(List(("gmosN", AbsentValue), ("gmosS", gmosS))) =>
          gmosS match
            case ObjectValue(
                  List(("grating", TypedEnumValue(EnumValue(d, _, _, _))),
                       ("fpu",
                        ObjectValue(
                          List(("customMask", AbsentValue),
                               ("builtin", TypedEnumValue(EnumValue(fpu, _, _, _)))
                          )
                        )
                       ),
                       ("filter", f)
                  )
                ) =>
              val filterOpt = f match
                case TypedEnumValue(EnumValue(f, _, _, _)) =>
                  gsFilter.get(f)
                case _                                     => none

              (gsGrating.get(d), gsFpu.get(fpu)).mapN((a, b) =>
                GmosSITCParams(a, GmosSouthFpuParam(b), filterOpt)
              )
            case _ =>
              none

        case ObjectValue(List(("gmosN", gmosN), ("gmosS", AbsentValue))) =>
          gmosN match
            case ObjectValue(
                  List(("grating", TypedEnumValue(EnumValue(d, _, _, _))),
                       ("fpu",
                        ObjectValue(
                          List(("customMask", AbsentValue),
                               ("builtin", TypedEnumValue(EnumValue(fpu, _, _, _)))
                          )
                        )
                       ),
                       ("filter", f)
                  )
                ) =>
              val filterOpt = f match
                case TypedEnumValue(EnumValue(f, _, _, _)) =>
                  gnFilter.get(f)
                case _                                     => none

              (gnGrating.get(d), gnFpu.get(fpu)).mapN((a, b) =>
                GmosNITCParams(a, GmosNorthFpuParam(b), filterOpt)
              )
            case _ =>
              none

      }.flatten
      cursorEnvAdd("modes", modes)(i)

  def instrumentModePartial: PartialFunction[(Partial, (String, Value)), Partial] =
    case (i, ("mode", m)) =>
      val modes = m match {
        case ObjectValue(List(("gmosN", AbsentValue), ("gmosS", gmosS))) =>
          gmosS match
            case ObjectValue(
                  List(("grating", TypedEnumValue(EnumValue(d, _, _, _))),
                       ("fpu",
                        ObjectValue(
                          List(("customMask", AbsentValue),
                               ("builtin", TypedEnumValue(EnumValue(fpu, _, _, _)))
                          )
                        )
                       ),
                       ("filter", f)
                  )
                ) =>
              val filterOpt = f match
                case TypedEnumValue(EnumValue(f, _, _, _)) =>
                  gsFilter.get(f)
                case _                                     => none

              (gsGrating.get(d), gsFpu.get(fpu)).mapN((a, b) =>
                GmosSITCParams(a, GmosSouthFpuParam(b), filterOpt)
              )
            case _ =>
              none

        case ObjectValue(List(("gmosN", gmosN), ("gmosS", AbsentValue))) =>
          gmosN match
            case ObjectValue(
                  List(("grating", TypedEnumValue(EnumValue(d, _, _, _))),
                       ("fpu",
                        ObjectValue(
                          List(("customMask", AbsentValue),
                               ("builtin", TypedEnumValue(EnumValue(fpu, _, _, _)))
                          )
                        )
                       ),
                       ("filter", f)
                  )
                ) =>
              val filterOpt = f match
                case TypedEnumValue(EnumValue(f, _, _, _)) =>
                  gnFilter.get(f)
                case _                                     => none

              (gnGrating.get(d), gnFpu.get(fpu)).mapN((a, b) =>
                GmosNITCParams(a, GmosNorthFpuParam(b), filterOpt)
              )
            case _ =>
              none

        case _ => none
      }
      modes
        .map(m => cursorEnvAdd("mode", m)(i))
        .getOrElse(i.addProblem("Mode couldn't be parsed"))

  def constraintsPartial: PartialFunction[(Partial, (String, Value)), Partial] =
    case (i,
          ("constraints",
           ObjectValue(
             List(("imageQuality", TypedEnumValue(EnumValue(iq, _, _, _))),
                  ("cloudExtinction", TypedEnumValue(EnumValue(ce, _, _, _))),
                  ("skyBackground", TypedEnumValue(EnumValue(sb, _, _, _))),
                  ("waterVapor", TypedEnumValue(EnumValue(wv, _, _, _))),
                  ("elevationRange",
                   ObjectValue(
                     List(
                       ("airMass", ObjectValue(List(("min", min), ("max", max)))),
                       ("hourAngle", AbsentValue)
                     )
                   )
                  )
             )
           )
          )
        ) =>
      val airMass: IorNec[String, BigDecimal] =
        (bigDecimalValue(min), bigDecimalValue(max))
          .mapN { (min, max) =>
            if (max >= min)
              max.rightIor.map { m =>
                AirMassBuckets.find(m <= _).getOrElse(AirMassBuckets.last)
              }
            else s"Airmass max value $max must be more than the min value $min".leftIorNec
          }
          .getOrElse("Missing airmass values".leftIorNec)

      val imageQuality: IorNec[String, ImageQuality] =
        iqItems.get(iq).toRightIorNec("Cannot parse image quality")

      val cloudExtinction: IorNec[String, CloudExtinction] =
        ceItems.get(ce).toRightIorNec("Cannot parse cloud extinction")

      val waterVapor    = wvItems.get(wv).toRightIorNec("Cannot parse water vapor")
      val skyBackground = sbItems.get(sb).toRightIorNec("Cannot parse sky background")
      (airMass, imageQuality, cloudExtinction, waterVapor, skyBackground)
        .parMapN { (am, iq, ce, wv, sb) =>
          cursorEnvAdd("constraints", ItcObservingConditions(iq, ce, wv, sb, am.toDouble))(
            i
          )
        }
        .leftProblems
        .flatten

    case (i,
          ("constraints",
           ObjectValue(
             List(
               ("imageQuality", TypedEnumValue(EnumValue(iq, _, _, _))),
               ("cloudExtinction", TypedEnumValue(EnumValue(ce, _, _, _))),
               ("skyBackground", TypedEnumValue(EnumValue(sb, _, _, _))),
               ("waterVapor", TypedEnumValue(EnumValue(wv, _, _, _))),
               ("elevationRange",
                ObjectValue(
                  List(
                    ("airMass", AbsentValue),
                    ("hourAngle", ObjectValue(List(("minHours", min), ("maxHours", max))))
                  )
                )
               )
             )
           )
          )
        ) =>
      val hourAngle: IorNec[String, BigDecimal] =
        (bigDecimalValue(min), bigDecimalValue(max))
          .mapN { (min, max) =>
            if (max >= min) max.rightIor
            else s"Hour Angle max value $max must be more than the min value $min".leftIorNec
          }
          .getOrElse("Missing Hour Angle values".leftIorNec)

      val imageQuality: IorNec[String, ImageQuality] =
        iqItems.get(iq).toRightIorNec("Cannot parse image quality")

      val cloudExtinction: IorNec[String, CloudExtinction] =
        ceItems.get(ce).toRightIorNec("Cannot parse cloud extinction")

      val waterVapor    = wvItems.get(wv).toRightIorNec("Cannot parse water vapor")
      val skyBackground = sbItems.get(sb).toRightIorNec("Cannot parse sky background")
      (hourAngle, imageQuality, cloudExtinction, waterVapor, skyBackground)
        .parMapN { (am, iq, ce, wv, sb) =>
          cursorEnvAdd("constraints", ItcObservingConditions(iq, ce, wv, sb, am.toDouble))(
            i
          )
        }
        .leftProblems
        .flatten
