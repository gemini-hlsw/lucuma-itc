// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.option.*
import cats.syntax.parallel.*
import grackle.Result
import lucuma.core.enums.SkyBackground
import lucuma.core.enums.WaterVapor
import lucuma.core.model.CloudExtinction
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ImageQuality
import lucuma.odb.graphql.binding.*
import lucuma.odb.graphql.binding.BigDecimalBinding
import lucuma.odb.graphql.input.ConstraintSetInput.NominalConstraints

final case class ImageQualityInput(
  preset: Option[ImageQuality.Preset],
  arcsec: Option[BigDecimal]
) {
  def toImageQuality: Result[ImageQuality] =
    (preset, arcsec) match {
      case (Some(p), None)    => Result(p.toImageQuality)
      case (None, Some(a))    =>
        ImageQuality.fromArcSeconds(a) match {
          case Right(iq) => Result(iq)
          case Left(err) => Result.failure(s"Invalid image quality value: $err")
        }
      case (Some(_), Some(_)) =>
        Result.failure("Cannot specify both preset and arcsec for ImageQuality")
      case (None, None)       => Result.failure("Must specify either preset or arcsec for ImageQuality")
    }
}

final case class CloudExtinctionInput(
  preset:     Option[CloudExtinction.Preset],
  extinction: Option[BigDecimal]
) {
  def toCloudExtinction: Result[CloudExtinction] =
    (preset, extinction) match {
      case (Some(p), None)    => Result(p.toCloudExtinction)
      case (None, Some(e))    =>
        CloudExtinction.fromVegaMagnitude(e) match {
          case Right(ce) => Result(ce)
          case Left(err) => Result.failure(s"Invalid cloud extinction value: $err")
        }
      case (Some(_), Some(_)) =>
        Result.failure("Cannot specify both preset and extinction for CloudExtinction")
      case (None, None)       =>
        Result.failure("Must specify either preset or extinction for CloudExtinction")
    }
}

final case class ItcConstraintsInput(
  cloudExtinction: Option[CloudExtinctionInput],
  imageQuality:    Option[ImageQualityInput],
  skyBackground:   Option[SkyBackground],
  waterVapor:      Option[WaterVapor],
  elevationRange:  Option[ElevationRangeInput]
) {

  def create: Result[ConstraintSet] = {
    // Use existing validation methods that properly handle mutual exclusion and range validation
    val iqResult: Result[ImageQuality.Preset] = imageQuality match {
      case Some(iqi) => iqi.toImageQuality.map(findClosestImageQualityPreset)
      case None      => Result(NominalConstraints.imageQuality)
    }

    val ceResult: Result[CloudExtinction.Preset] = cloudExtinction match {
      case Some(cei) => cei.toCloudExtinction.map(findClosestCloudExtinctionPreset)
      case None      => Result(NominalConstraints.cloudExtinction)
    }

    val sb = skyBackground.getOrElse(NominalConstraints.skyBackground)
    val wv = waterVapor.getOrElse(NominalConstraints.waterVapor)
    val erResult = elevationRange.fold(Result(NominalConstraints.elevationRange))(_.create)

    // Combine all validation results - if any fail, the whole operation fails
    (iqResult, ceResult, erResult).parMapN { (iq, ce, er) =>
      ConstraintSet(iq, ce, sb, wv, er)
    }
  }

  private def findClosestCloudExtinctionPreset(ce: CloudExtinction): CloudExtinction.Preset = {
    val targetVegaMag = ce.toVegaMagnitude

    ItcConstraintsInput.AllCloudExtinctionPresets.minBy { preset =>
      (preset.toCloudExtinction.toVegaMagnitude - targetVegaMag).abs
    }
  }

  private def findClosestImageQualityPreset(iq: ImageQuality): ImageQuality.Preset = {
    val targetArcsec = iq.toArcSeconds

    ItcConstraintsInput.AllImageQualityPresets.minBy { preset =>
      (preset.toImageQuality.toArcSeconds - targetArcsec).abs
    }
  }

}

object ItcConstraintsInput {

  // Cache enumerated values for performance
  private lazy val AllCloudExtinctionPresets: List[CloudExtinction.Preset] =
    lucuma.core.util.Enumerated[CloudExtinction.Preset].all

  private lazy val AllImageQualityPresets: List[ImageQuality.Preset] =
    lucuma.core.util.Enumerated[ImageQuality.Preset].all

  val Default: ItcConstraintsInput =
    ItcConstraintsInput(
      CloudExtinctionInput(NominalConstraints.cloudExtinction.some, None).some,
      ImageQualityInput(NominalConstraints.imageQuality.some, None).some,
      NominalConstraints.skyBackground.some,
      NominalConstraints.waterVapor.some,
      ElevationRangeInput.Default.some
    )

  val CloudExtinctionPresetBinding: Matcher[CloudExtinction.Preset] =
    enumeratedBinding[CloudExtinction.Preset]

  val ImageQualityPresetBinding: Matcher[ImageQuality.Preset] =
    enumeratedBinding[ImageQuality.Preset]

  val SkyBackgroundBinding: Matcher[SkyBackground] =
    enumeratedBinding[SkyBackground]

  val WaterVaporBinding: Matcher[WaterVapor] =
    enumeratedBinding[WaterVapor]

  val ImageQualityInputBinding: Matcher[ImageQualityInput] =
    ObjectFieldsBinding.rmap {
      case List(
            ImageQualityPresetBinding.Option("preset", rPreset),
            BigDecimalBinding.Nullable("arcsec", rArcsec)
          ) =>
        (rPreset, rArcsec.map(_.toOption)).parMapN(ImageQualityInput(_, _))
    }

  val CloudExtinctionInputBinding: Matcher[CloudExtinctionInput] =
    ObjectFieldsBinding.rmap {
      case List(
            CloudExtinctionPresetBinding.Option("preset", rPreset),
            BigDecimalBinding.Nullable("extinction", rExtinction)
          ) =>
        (rPreset, rExtinction.map(_.toOption)).parMapN(CloudExtinctionInput(_, _))
    }

  val Binding: Matcher[ItcConstraintsInput] =
    ObjectFieldsBinding.rmap {
      case List(
            ImageQualityInputBinding.Option("imageQuality", rImage),
            CloudExtinctionInputBinding.Option("cloudExtinction", rCloud),
            SkyBackgroundBinding.Option("skyBackground", rSky),
            WaterVaporBinding.Option("waterVapor", rWater),
            ElevationRangeInput.Binding.Option("elevationRange", rElevation)
          ) =>
        (rCloud, rImage, rSky, rWater, rElevation).parMapN(ItcConstraintsInput(_, _, _, _, _))
    }

}
