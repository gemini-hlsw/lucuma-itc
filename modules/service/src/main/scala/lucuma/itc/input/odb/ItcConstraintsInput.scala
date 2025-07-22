// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import cats.syntax.flatMap.*
import grackle.Result
import lucuma.core.enums.SkyBackground
import lucuma.core.enums.WaterVapor
import lucuma.core.model.CloudExtinction
import lucuma.core.model.ImageQuality
import lucuma.odb.graphql.binding.*
import lucuma.odb.graphql.input.*
import lucuma.odb.graphql.binding.BigDecimalBinding
import lucuma.odb.graphql.input.ConstraintSetInput.NominalConstraints
import lucuma.core.util.NewType
import lucuma.itc.service.ItcObservingConditions

object ImageQualityInput extends NewType[Either[ImageQuality.Preset, BigDecimal]]:
  def preset(p: ImageQuality.Preset): ImageQualityInput = ImageQualityInput(Left(p))
  def arcsec(a: BigDecimal): ImageQualityInput          = ImageQualityInput(Right(a))

  extension (iqi: ImageQualityInput) {
    def toImageQuality: Result[ImageQuality] =
      iqi.value match
        case Left(preset)  => Result(preset.toImageQuality)
        case Right(arcsec) =>
          ImageQuality.fromArcSeconds(arcsec) match
            case Right(iq) => Result(iq)
            case Left(err) => Result.failure(s"Invalid image quality value: $err")
  }

type ImageQualityInput = ImageQualityInput.Type

object CloudExtinctionInput extends NewType[Either[CloudExtinction.Preset, BigDecimal]]:
  def preset(p:     CloudExtinction.Preset): CloudExtinctionInput = CloudExtinctionInput(Left(p))
  def extinction(e: BigDecimal): CloudExtinctionInput             = CloudExtinctionInput(Right(e))

  extension (cei: CloudExtinctionInput) {
    def toCloudExtinction: Result[CloudExtinction] =
      cei.value match
        case Left(preset)      => Result(preset.toCloudExtinction)
        case Right(extinction) =>
          CloudExtinction.fromVegaMagnitude(extinction) match
            case Right(ce) => Result(ce)
            case Left(err) => Result.failure(s"Invalid cloud extinction value: $err")
  }

type CloudExtinctionInput = CloudExtinctionInput.Type

case class ItcConstraintsInput(
  cloudExtinction: CloudExtinctionInput,
  imageQuality:    ImageQualityInput,
  skyBackground:   SkyBackground,
  waterVapor:      WaterVapor,
  elevationRange:  ElevationRangeInput
) {

  def create: Result[ItcObservingConditions] = {
    import ImageQualityInput.*
    import CloudExtinctionInput.*
    val iqResult: Result[ImageQuality.Preset]    =
      imageQuality.toImageQuality.map(findClosestImageQualityPreset)
    val ceResult: Result[CloudExtinction.Preset] =
      cloudExtinction.toCloudExtinction.map(findClosestCloudExtinctionPreset)
    val erResult: Result[BigDecimal]             =
      elevationRange.create.flatMap(e => Result.fromEither(ItcObservingConditions.airmass(e)))

    // Combine all validation results - if any fail, the whole operation fails
    (iqResult, ceResult, erResult).parMapN { (iq, ce, er) =>
      ItcObservingConditions(iq, ce, waterVapor, skyBackground, er.toDouble)
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
      CloudExtinctionInput.preset(NominalConstraints.cloudExtinction),
      ImageQualityInput.preset(NominalConstraints.imageQuality),
      NominalConstraints.skyBackground,
      NominalConstraints.waterVapor,
      ElevationRangeInput.Default
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
            BigDecimalBinding.Option("arcsec", rArcsec)
          ) =>
        (rPreset, rArcsec).parFlatMapN { (presetOpt, arcsecOpt) =>
          oneOrFail(
            presetOpt -> "preset",
            arcsecOpt -> "arcsec"
          ).map {
            case p: ImageQuality.Preset => ImageQualityInput.preset(p)
            case b: BigDecimal          => ImageQualityInput.arcsec(b)
          }
        }
    }

  val CloudExtinctionInputBinding: Matcher[CloudExtinctionInput] =
    ObjectFieldsBinding.rmap {
      case List(
            CloudExtinctionPresetBinding.Option("preset", rPreset),
            BigDecimalBinding.Option("extinction", rExtinction)
          ) =>
        (rPreset, rExtinction).parMapN { (presetOpt, extinctionOpt) =>
          oneOrFail(
            presetOpt     -> "preset",
            extinctionOpt -> "extinction"
          ).map {
            case p: CloudExtinction.Preset => CloudExtinctionInput.preset(p)
            case b: BigDecimal             => CloudExtinctionInput.extinction(b)
          }
        }.flatten
    }

  val Binding: Matcher[ItcConstraintsInput] =
    ObjectFieldsBinding.rmap {
      case List(
            ImageQualityInputBinding("imageQuality", rImage),
            CloudExtinctionInputBinding("cloudExtinction", rCloud),
            SkyBackgroundBinding("skyBackground", rSky),
            WaterVaporBinding("waterVapor", rWater),
            ElevationRangeInput.Binding("elevationRange", rElevation)
          ) =>
        (rCloud, rImage, rSky, rWater, rElevation).parMapN(ItcConstraintsInput.apply)
    }

}
