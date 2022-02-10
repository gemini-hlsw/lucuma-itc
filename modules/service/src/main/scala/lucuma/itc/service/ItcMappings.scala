// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import cats._
import cats.data._
import cats.effect.{ Unique => _, _ }
import cats.syntax.all._
import coulomb._
import coulomb.si.Kelvin
import coulomb.si._
import coulomb.siprefix._
import coulomb.time._
import edu.gemini.grackle._
import edu.gemini.grackle.circe.CirceMapping
import eu.timepit.refined._
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.types.numeric.PosBigDecimal
import io.circe.Encoder
import io.circe.Json
import lucuma.core.enum.SkyBackground
import lucuma.core.enum.WaterVapor
import lucuma.core.enum._
import lucuma.core.math.Angle
import lucuma.core.math.BrightnessUnits._
import lucuma.core.math.BrightnessValue
import lucuma.core.math.RadialVelocity
import lucuma.core.math.Wavelength
import lucuma.core.math.dimensional.Units._
import lucuma.core.math.dimensional._
import lucuma.core.math.units._
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.UnnormalizedSED
import lucuma.core.model.UnnormalizedSED._
import lucuma.core.syntax.enumerated._
import lucuma.core.syntax.string._
import lucuma.core.util.Enumerated
import lucuma.itc.Itc
import lucuma.itc.ItcObservingConditions
import lucuma.itc.search.ObservingMode
import lucuma.itc.search.ObservingMode.Spectroscopy.GmosNorth
import lucuma.itc.search.ObservingMode.Spectroscopy.GmosSouth
import lucuma.itc.search.Result.Spectroscopy
import lucuma.itc.search.SpectroscopyResults
import lucuma.itc.search.TargetProfile
import lucuma.itc.service.syntax.all._
import natchez.Trace
import org.typelevel.log4cats.Logger

import java.math.RoundingMode
import scala.collection.immutable.SortedMap
import scala.concurrent.duration._
import scala.io.Source
import scala.util.Using

import Query._
import Value._
import QueryCompiler._

trait Encoders {
  import io.circe.generic.semiauto._
  import io.circe.syntax._
  type Nanosecond  = Nano %* Second
  type Microsecond = Micro %* Second
  type Millisecond = Milli %* Second

  implicit val encoderFiniteDuration: Encoder[FiniteDuration] = new Encoder[FiniteDuration] {
    type Micro
    final def apply(d: FiniteDuration): Json = {
      val value = d.toNanos.withUnit[Nanosecond]
      Json.obj(
        ("microseconds", Json.fromLong(value.toUnit[Microsecond].value)),
        ("milliseconds", Json.fromBigDecimal(value.to[BigDecimal, Millisecond].value)),
        ("seconds", Json.fromBigDecimal(value.to[BigDecimal, Second].value)),
        ("minutes", Json.fromBigDecimal(value.to[BigDecimal, Minute].value)),
        ("hours", Json.fromBigDecimal(value.to[BigDecimal, Hour].value))
      )
    }
  }

  implicit val encoderItcResultSuccess: Encoder[Itc.Result.Success] =
    deriveEncoder[Itc.Result.Success]

  implicit val encoderItcResult: Encoder[Itc.Result] = Encoder.instance {
    case f: Itc.Result.Success          =>
      Json.obj(("resultType", Json.fromString("Success"))).deepMerge(f.asJson)
    case Itc.Result.CalculationError(m) =>
      Json.obj(("resultType", Json.fromString("Error")), ("msg", Json.fromString(m)))
    case Itc.Result.SourceTooBright(m)  =>
      Json.obj(("resultType", Json.fromString("Error")),
               ("msg", Json.fromString(s"Source too bright $m"))
      )
  }

  implicit val encoderWavelength: Encoder[Wavelength] = new Encoder[Wavelength] {
    final def apply(w: Wavelength): Json = Json.obj(
      ("picometers", Json.fromInt(w.toPicometers.value.value)),
      ("angstrom", Json.fromBigDecimal(w.angstrom.value.toBigDecimal(2, RoundingMode.CEILING))),
      ("nanometers",
       Json.fromBigDecimal(
         w.nanometer.value.toBigDecimal(2, RoundingMode.CEILING)
       )
      ),
      ("micrometers",
       Json.fromBigDecimal(
         w.micrometer.value.toBigDecimal(2, RoundingMode.CEILING)
       )
      )
    )
  }

  implicit val encoderGmosNITCParams: Encoder[GmosNITCParams] =
    deriveEncoder[GmosNITCParams]

  implicit val encoderGmosSITCParams: Encoder[GmosSITCParams] =
    deriveEncoder[GmosSITCParams]

  implicit val encoderGmosNorth: Encoder[GmosNorth] = new Encoder[GmosNorth] {
    final def apply(a: GmosNorth): Json = Json.obj(
      ("instrument", Json.fromString(a.instrument.longName.toUpperCase.replace(" ", "_"))),
      ("resolution", Json.fromInt(a.resolution.toInt)),
      ("params", GmosNITCParams(a.disperser, a.fpu, a.filter).asJson),
      ("wavelength", a.λ.asJson)
    )
  }

  implicit val encoderGmosSouth: Encoder[GmosSouth] = new Encoder[GmosSouth] {
    final def apply(a: GmosSouth): Json = Json.obj(
      ("instrument", Json.fromString(a.instrument.longName.toUpperCase.replace(" ", "_"))),
      ("resolution", Json.fromInt(a.resolution.toInt)),
      ("params", GmosSITCParams(a.disperser, a.fpu, a.filter).asJson),
      ("wavelength", a.λ.asJson)
    )
  }

  implicit val encoderObservingMode: Encoder[ObservingMode.Spectroscopy] = Encoder.instance {
    case gn: GmosNorth => gn.asJson
    case gs: GmosSouth => gs.asJson
  }

  implicit val encoderSpectroscopy: Encoder[Spectroscopy] = new Encoder[Spectroscopy] {
    final def apply(s: Spectroscopy): Json = Json.obj(
      ("mode", s.mode.asJson),
      ("itc", s.itc.asJson)
    )
  }

  implicit val encoderSpectroscopyResults: Encoder[SpectroscopyResults] =
    deriveEncoder[SpectroscopyResults]

}

sealed trait SpectroscopyParams

final case class GmosNITCParams(
  disperser: GmosNorthDisperser,
  fpu:       GmosNorthFpu,
  filter:    Option[GmosNorthFilter]
) extends SpectroscopyParams

final case class GmosSITCParams(
  disperser: GmosSouthDisperser,
  fpu:       GmosSouthFpu,
  filter:    Option[GmosSouthFilter]
) extends SpectroscopyParams

object ItcMapping extends Encoders {

  def enumTags[A: Enumerated] =
    Enumerated[A].all.fproductLeft(_.tag.toScreamingSnakeCase).toMap

  val gnFilter    = enumTags[GmosNorthFilter]
  val gnDisperser = enumTags[GmosNorthDisperser]
  val gnFpu       = enumTags[GmosNorthFpu]

  val gsFilter    = enumTags[GmosSouthFilter]
  val gsDisperser = enumTags[GmosSouthDisperser]
  val gsFpu       = enumTags[GmosSouthFpu]

  val iqItems = enumTags[ImageQuality]
  val ceItems = enumTags[CloudExtinction]
  val wvItems = enumTags[WaterVapor]
  val sbItems = enumTags[SkyBackground]

  val stellarLibraryItems  = enumTags[StellarLibrarySpectrum]
  val coolStarItems        = enumTags[CoolStarTemperature]
  val galaxyItems          = enumTags[GalaxySpectrum]
  val planetItems          = enumTags[PlanetSpectrum]
  val hiiRegionItems       = enumTags[HIIRegionSpectrum]
  val planetaryNebulaItems = enumTags[PlanetaryNebulaSpectrum]
  val quasarItems          = enumTags[QuasarSpectrum]

  val bandItems            = enumTags[Band]
  val integratedUnitsItems = enumTags[Units Of Brightness[Integrated]]
  val surfaceUnitsItems    = enumTags[Units Of Brightness[Surface]]

  // In principle this is a pure operation because resources are constant values, but the potential
  // for error in dev is high and it's nice to handle failures in `F`.
  def loadSchema[F[_]: Sync]: F[Schema] =
    Sync[F].defer {
      Using(Source.fromResource("graphql/itc.graphql", getClass().getClassLoader())) { src =>
        Schema(src.mkString).right.get
      }.liftTo[F]
    }

  def spectroscopy[F[_]: ApplicativeError[*[_], Throwable]: Logger: Parallel: Trace](
    itc: Itc[F]
  )(env: Cursor.Env): F[Result[List[SpectroscopyResults]]] =
    (env.get[Wavelength]("wavelength"),
     env.get[RadialVelocity]("radialVelocity").flatMap(_.toRedshift),
     env.get[PosBigDecimal]("signalToNoise"),
     env.get[SourceProfile]("sourceProfile"),
     env.get[Band]("band"),
     env.get[List[SpectroscopyParams]]("modes"),
     env.get[ItcObservingConditions]("constraints")
    ).traverseN { (wv, rs, sn, sp, sd, modes, c) =>
      modes
        .parTraverse { mode =>
          Logger[F].info(s"ITC calculate for $mode, conditions $c and profile $sp") *>
            Trace[F].put(("itc.modes_count", modes.length)) *> {
              val specMode = mode match {
                case GmosNITCParams(disperser, fpu, filter) =>
                  ObservingMode.Spectroscopy.GmosNorth(wv, disperser, fpu, filter)
                case GmosSITCParams(disperser, fpu, filter) =>
                  ObservingMode.Spectroscopy.GmosSouth(wv, disperser, fpu, filter)
              }
              itc
                .calculate(
                  TargetProfile(sp, sd, rs),
                  specMode,
                  c,
                  sn.value
                )
                .handleErrorWith { case x =>
                  Logger[F].error(x)(s"Upstream error") *>
                    Itc.Result.CalculationError(s"Error calculating itc $x").pure[F].widen
                }
                .map(r => SpectroscopyResults(List(Spectroscopy(specMode, r))))
            }
        }
        .map(_.rightIor[NonEmptyChain[Problem]])
        .handleErrorWith { case x =>
          Problem(s"Error calculating itc $x").leftIorNec.pure[F]
        }
    }.map(_.getOrElse((Problem("Missing parameters for spectroscopy")).leftIorNec))

  def bigDecimalValue(v: Value): Option[BigDecimal] = v match {
    case IntValue(r)    => BigDecimal(r).some
    case FloatValue(r)  => BigDecimal(r).some
    case StringValue(r) => Either.catchNonFatal(BigDecimal(r)).toOption
    case _              => none
  }

  def parseFwhw(units: List[(String, Value)]): Option[Angle] =
    units.find(_._2 != Value.AbsentValue) match {
      case Some(("microarcseconds", n)) =>
        bigDecimalValue(n).map(n => Angle.microarcseconds.reverseGet(n.toLong))
      case Some(("milliarcseconds", n)) =>
        bigDecimalValue(n).map(n => Angle.milliarcseconds.reverseGet(n.toInt))
      case Some(("arcseconds", n))      =>
        bigDecimalValue(n).map(n => Angle.arcseconds.reverseGet(n.toInt))
      case _                            => None
    }

  def parseWavelength(units: List[(String, Value)]): Option[Wavelength] =
    units.find(_._2 != Value.AbsentValue) match {
      case Some(("picometers", IntValue(n))) =>
        Wavelength.fromPicometers.getOption(n)
      case Some(("angstroms", n))            =>
        bigDecimalValue(n).flatMap(Wavelength.decimalAngstroms.getOption)
      case Some(("nanometers", n))           =>
        bigDecimalValue(n).flatMap(Wavelength.decimalNanometers.getOption)
      case Some(("micrometers", n))          =>
        bigDecimalValue(n).flatMap(Wavelength.decimalMicrometers.getOption)
      case _                                 => None
    }

  def parseRadialVelocity(units: List[(String, Value)]): Option[RadialVelocity] =
    units.find(_._2 != Value.AbsentValue) match {
      case Some(("centimetersPerSecond", IntValue(n))) =>
        RadialVelocity(n.withUnit[CentimetersPerSecond].to[BigDecimal, MetersPerSecond])
      case Some(("metersPerSecond", n))                =>
        bigDecimalValue(n).flatMap(v => RadialVelocity(v.withUnit[MetersPerSecond]))
      case Some(("kilometersPerSecond", n))            =>
        bigDecimalValue(n).flatMap(v => RadialVelocity.kilometerspersecond.getOption(v))
      case _                                           => None
    }

  def apply[F[_]: Sync: Logger: Parallel: Trace](itc: Itc[F]): F[Mapping[F]] =
    loadSchema[F].map { loadedSchema =>
      new CirceMapping[F] with ComputeMapping[F] {

        val schema: Schema         = loadedSchema
        val QueryType              = schema.ref("Query")
        val SpectroscopyResultType = schema.ref("SpectroscopyResult")
        val BigDecimalType         = schema.ref("BigDecimal")
        val LongType               = schema.ref("Long")
        val DurationType           = schema.ref("Duration")
        val typeMappings           =
          List(
            ObjectMapping(
              tpe = QueryType,
              fieldMappings = List(
                ComputeRoot[List[SpectroscopyResults]]("spectroscopy",
                                                       ListType(SpectroscopyResultType),
                                                       spectroscopy[F](itc)
                )
              )
            ),
            LeafMapping[BigDecimal](BigDecimalType),
            LeafMapping[Long](LongType),
            LeafMapping[FiniteDuration](DurationType)
          )

        val AirMassBuckets = Vector(BigDecimal(1.2), BigDecimal(1.5), BigDecimal(2.0))

        def wavelengthPartial: PartialFunction[(IorNec[Problem, Environment], (String, Value)),
                                               IorNec[Problem, Environment]
        ] = {
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
        }

        def radialVelocityPartial: PartialFunction[(IorNec[Problem, Environment], (String, Value)),
                                                   IorNec[Problem, Environment]
        ] = {
          // radialVelocity
          case (i, ("radialVelocity", ObjectValue(r))) =>
            parseRadialVelocity(r) match {
              case Some(r) =>
                cursorEnvAdd("radialVelocity", r)(i)
              case _       =>
                i.addProblem(s"Radial Velocity value is not valid $r")
            }
        }

        def signalToNoisePartial: PartialFunction[(IorNec[Problem, Environment], (String, Value)),
                                                  IorNec[Problem, Environment]
        ] = {
          // signalToNoise
          case (i, ("signalToNoise", r)) =>
            bigDecimalValue(r) match {
              case Some(r) if r > 0 =>
                refineV[Positive](r)
                  .fold(i.addProblem, v => cursorEnvAdd("signalToNoise", v)(i))
              case Some(r)          =>
                i.addProblem(s"signalToNoise value $r must be positive")
              case _                =>
                i.addProblem(s"Not valid signalToNoise value $r")
            }
        }
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
                  b -> u.withValueTagged(BrightnessValue.fromBigDecimal.get(v))
                }
                .toRightIorNec("Invalid brightness")
            case e => s"Invalid brighness entry $e".leftIorNec
          }
          .sequence
          .map(v => SortedMap(v: _*))

        def sedReader(sed: List[(String, Value)]): IorNec[String, UnnormalizedSED] = sed match {
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
            bigDecimalValue(r) match {
              case Some(r) =>
                val powerLaw = UnnormalizedSED.PowerLaw(r)
                powerLaw.rightIorNec
              case _       =>
                s"Not a valid power law value $r".leftIorNec
            }
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
            bigDecimalValue(r) match {
              case Some(r) if r > 0 =>
                val blackBody = refineV[Positive](r)
                  .map(k => UnnormalizedSED.BlackBody(k.withUnit[Kelvin]))
                  .toOption
                blackBody.toRightIorNec(s"Not a valid black body value $r")
              case Some(r)          =>
                s"black body value $r must be positive".leftIorNec
              case _                =>
                s"Not a valid black body value $r".leftIorNec
            }
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
        }

        def sourceProfilePartial: PartialFunction[(IorNec[Problem, Environment], (String, Value)),
                                                  IorNec[Problem, Environment]
        ] = {
          case (i, ("sourceProfile", ObjectValue(v))) if v.length === 3 =>
            (v match {
              case ("point", AbsentValue) ::
                  ("uniform", ObjectValue(ov)) ::
                  ("gaussian", AbsentValue) :: Nil =>
                val uniform: IorNec[String, SourceProfile] = ov match {
                  case ("bandNormalized", ObjectValue(bn)) ::
                      ("emissionLines", AbsentValue) ::
                      Nil =>
                    val s: IorNec[String, SourceProfile.Uniform] = bn match {
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
                    }
                    s
                  case _ => "Emission lines are not supported yet".leftIorNec
                }
                uniform.map(p => cursorEnvAdd("sourceProfile", p)(i)).leftProblems.flatten
              case ("point", ObjectValue(ov)) ::
                  ("uniform", AbsentValue) ::
                  ("gaussian", AbsentValue) :: Nil =>
                val point: IorNec[String, SourceProfile] = ov match {
                  case ("bandNormalized", ObjectValue(bn)) ::
                      ("emissionLines", AbsentValue) ::
                      Nil =>
                    val s: IorNec[String, SourceProfile.Point] = bn match {
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
                    }
                    s
                  case _ => "Emission lines are not supported yet".leftIorNec
                }
                point.map(p => cursorEnvAdd("sourceProfile", p)(i)).leftProblems.flatten
              case ("point", AbsentValue) ::
                  ("uniform", AbsentValue) ::
                  ("gaussian", ObjectValue(ov)) :: Nil =>
                val gaussian: IorNec[String, SourceProfile] = ov match {
                  case ("fwhm", ObjectValue(fw)) ::
                      ("spectralDefinition", ObjectValue(sd)) ::
                      Nil =>
                    val fwhmResult = parseFwhw(fw).toRightIorNec(s"Cannot parse fwhm $fw")

                    val s: IorNec[String, SourceProfile.Gaussian] = sd match {
                      case ("bandNormalized", ObjectValue(bn)) ::
                          ("emissionLines", AbsentValue) ::
                          Nil =>
                        bn match {
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
                        }
                      case _ => "Emission lines are not supported yet".leftIorNec
                    }
                    s
                  case _ => "Cannot parse gaussian profile input".leftIorNec
                }
                gaussian.map(p => cursorEnvAdd("sourceProfile", p)(i)).leftProblems.flatten
              case _ => i.addProblem("Unsupported source profile")
            })
          case (i, ("sourceProfile", v))                                =>
            i.addProblem(s"Cannot parse sourceProfile $v")
        }

        def bandPartial: PartialFunction[(IorNec[Problem, Environment], (String, Value)),
                                         IorNec[Problem, Environment]
        ] = {
          // magnitude
          case (i, ("band", TypedEnumValue(EnumValue(b, _, _, _)))) =>
            bandItems
              .get(b)
              .map(b => cursorEnvAdd("band", b)(i))
              .getOrElse(i.addProblem("Cannot parse band"))
        }

        def instrumentModesPartial: PartialFunction[(IorNec[Problem, Environment], (String, Value)),
                                                    IorNec[Problem, Environment]
        ] = { case (i, ("modes", ListValue(m))) =>
          val modes = m.collect {
            case ObjectValue(List(("gmosN", AbsentValue), ("gmosS", gmosS))) =>
              gmosS match {
                case ObjectValue(
                      List(("disperser", TypedEnumValue(EnumValue(d, _, _, _))),
                           ("fpu", TypedEnumValue(EnumValue(fpu, _, _, _))),
                           ("filter", f)
                      )
                    ) =>
                  val filterOpt = f match {
                    case TypedEnumValue(EnumValue(f, _, _, _)) =>
                      gsFilter.get(f)
                    case _                                     => none
                  }
                  (gsDisperser.get(d), gsFpu.get(fpu)).mapN(GmosSITCParams(_, _, filterOpt))
                case _ =>
                  none
              }
            case ObjectValue(List(("gmosN", gmosN), ("gmosS", AbsentValue))) =>
              gmosN match {
                case ObjectValue(
                      List(("disperser", TypedEnumValue(EnumValue(d, _, _, _))),
                           ("fpu", TypedEnumValue(EnumValue(fpu, _, _, _))),
                           ("filter", f)
                      )
                    ) =>
                  val filterOpt = f match {
                    case TypedEnumValue(EnumValue(f, _, _, _)) =>
                      gnFilter.get(f)
                    case _                                     => none
                  }
                  (gnDisperser.get(d), gnFpu.get(fpu)).mapN(GmosNITCParams(_, _, filterOpt))
                case _ =>
                  none
              }
          }.flatten
          cursorEnvAdd("modes", modes)(i)
        }

        def constraintsPartial: PartialFunction[(IorNec[Problem, Environment], (String, Value)),
                                                IorNec[Problem, Environment]
        ] = {
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
                             ("airmassRange", ObjectValue(List(("min", min), ("max", max)))),
                             ("hourAngleRange", AbsentValue)
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
                          ("airmassRange", AbsentValue),
                          ("hourAngleRange",
                           ObjectValue(List(("minHours", min), ("maxHours", max)))
                          )
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
        }

        def fallback(a: (IorNec[Problem, Environment], (String, Value))) =
          a._1.addProblem(s"Unexpected param ${a._2._1}")

        override val selectElaborator =
          new SelectElaborator(
            Map(
              QueryType -> {
                case Select("spectroscopy", List(Binding("input", ObjectValue(wv))), child) =>
                  wv.foldLeft(Environment(Cursor.Env(), child).rightIor[NonEmptyChain[Problem]]) {
                    case (e, c) =>
                      wavelengthPartial
                        .orElse(radialVelocityPartial)
                        .orElse(signalToNoisePartial)
                        .orElse(sourceProfilePartial)
                        .orElse(bandPartial)
                        .orElse(instrumentModesPartial)
                        .orElse(constraintsPartial)
                        .applyOrElse(
                          (e, c),
                          fallback
                        )
                  }.map(e => e.copy(child = Select("spectroscopy", Nil, child)))
              }
            )
          )
      }
    }
}
