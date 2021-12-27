// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

// import coulomb.define.UnitDefinition
import io.circe.Encoder
import io.circe.Json
import io.circe.generic.semiauto._
import io.circe.syntax._
import lucuma.core.enum._
import lucuma.core.math.Angle
import lucuma.core.math.Redshift
import lucuma.itc.search.TargetProfile
import lucuma.itc.search.syntax.sed._
import lucuma.core.math.BrightnessValue
import lucuma.core.model.SourceProfile
import lucuma.core.model.UnnormalizedSED

final case class ItcSourceDefinition(
  profile:      SourceProfile,
  distribution: UnnormalizedSED,
  norm:         BrightnessValue,
  // units:        UnitDefinition,
  // units:        Either[MagnitudeSystem, SurfaceBrightness],
  normBand:     Band,
  redshift:     Redshift
)

object ItcSourceDefinition {

  def fromTargetProfile(p: TargetProfile): ItcSourceDefinition =
    ItcSourceDefinition(
      p.sourceProfile,
      SourceProfile.unnormalizedSED
        .getOption(p.sourceProfile)
        .getOrElse(sys.error("Needs to have a SED defined")),
      p.brightness.getOrElse(sys.error("Needs to have a brightness defined")),
      // Nothing,
      // p.brightnessUnits.getOrElse(sys.error("Needs to have a brightness units defined")),
      // p.sourceProfile match {
      //   case SpatialProfile.GaussianSource(_) => Left(p.magnitude.system)
      //   case SpatialProfile.PointSource       => Left(p.magnitude.system)
      //   case SpatialProfile.UniformSource     =>
      //     Right {
      //       p.magnitude.system match {
      //         case MagnitudeSystem.Vega           => SurfaceBrightness.Vega
      //         case MagnitudeSystem.AB             => SurfaceBrightness.AB
      //         case MagnitudeSystem.Jy             => SurfaceBrightness.Jy
      //         case MagnitudeSystem.Watts          => SurfaceBrightness.Watts
      //         case MagnitudeSystem.ErgsFrequency  => SurfaceBrightness.ErgsFrequency
      //         case MagnitudeSystem.ErgsWavelength => SurfaceBrightness.ErgsWavelength
      //       }
      //     }
      // },
      p.band,
      p.redshift
    )

  implicit val sourceProfileEncoder: Encoder[SourceProfile] =
    new Encoder[SourceProfile] {
      import SourceProfile._
      def apply(a: SourceProfile): Json =
        a match {
          case _: Point    => Json.obj("PointSource" -> Json.obj())
          case _: Uniform  => Json.obj("UniformSource" -> Json.obj())
          case g: Gaussian =>
            Json.obj(
              "GaussianSource" -> Json.obj(
                "fwhm" -> Angle.signedDecimalArcseconds.get(g.fwhm).asJson
              )
            )
        }
    }

  implicit val spectralDistributionEncoder: Encoder[UnnormalizedSED] =
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
          case Galaxy(s)          =>
            Json.obj("Library" -> Json.obj("LibraryNonStar" -> Json.fromString(s.ocs2Tag)))
          case Planet(s)          =>
            Json.obj("Library" -> Json.obj("LibraryNonStar" -> Json.fromString(s.ocs2Tag)))
          case HIIRegion(s)       =>
            Json.obj("Library" -> Json.obj("LibraryNonStar" -> Json.fromString(s.ocs2Tag)))
          case PlanetaryNebula(s) =>
            Json.obj("Library" -> Json.obj("LibraryNonStar" -> Json.fromString(s.ocs2Tag)))
          case Quasar(s)          =>
            Json.obj("Library" -> Json.obj("LibraryNonStar" -> Json.fromString(s.ocs2Tag)))
          case s: CoolStarModel   =>
            Json.obj("Library" -> Json.obj("LibraryNonStar" -> Json.fromString(s.ocs2Tag)))
          case _                  => // TODO CoolStar and UserDefined
            Json.obj("Library" -> Json.Null)
        }
    }

  // implicit val unitEncoder: Encoder[UnitDefinition]             = ???
  // implicit val unitEncoder: Encoder[Either[MagnitudeSystem, SurfaceBrightness]] =
  //   new Encoder[Either[MagnitudeSystem, SurfaceBrightness]] {
  //     def apply(a: Either[MagnitudeSystem, SurfaceBrightness]): Json =
  //       a match {
  //         case Left(ms)  => Json.obj("MagnitudeSystem" -> Json.fromString(ms.tag))
  //         case Right(sb) => Json.obj("SurfaceBrightness" -> Json.fromString(sb.ocs2Tag))
  //       }
  //   }
  //
  implicit val brightnessValueEncoder: Encoder[BrightnessValue] =
    Encoder[BigDecimal].contramap(BrightnessValue.fromBigDecimal.reverseGet)

  implicit val bandEncoder: Encoder[Band] =
    Encoder[String].contramap(_.shortName)

  implicit val redshiftEncoder: Encoder[Redshift] =
    Encoder.forProduct1("z")(_.z)

  implicit val encoder: Encoder[ItcSourceDefinition] =
    deriveEncoder

}
