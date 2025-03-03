// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.input.customSed

import cats.data.NonEmptyMap
import eu.timepit.refined.types.numeric.PosBigDecimal
import lucuma.core.math.Wavelength
import lucuma.core.model.Attachment
import lucuma.itc.search.TargetData
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition.BandNormalized
import cats.Monad
import cats.syntax.all.*
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.UnnormalizedSED
import lucuma.itc.service.requests.TargetGraphRequest
import lucuma.itc.service.requests.TargetSpectroscopyTimeRequest
import lucuma.itc.service.requests.TargetImagingTimeRequest

object CustomSed:
  trait Resolver[F[_]]:
    def resolve(id: Attachment.Id): F[NonEmptyMap[Wavelength, PosBigDecimal]]

  object Resolver:
    def apply[F[_]](using r: Resolver[F]): Resolver[F] = r

  def resolveTargetData[F[_]: Monad: Resolver]: TargetData => F[TargetData] =
    case TargetData(
          SourceProfile.Point(
            SpectralDefinition.BandNormalized(
              Some(UnnormalizedSED.UserDefinedAttachment(id)),
              brightnesses
            )
          ),
          redshift
        ) =>
      Resolver[F]
        .resolve(id)
        .map: customSed =>
          TargetData(
            SourceProfile.Point(
              SpectralDefinition.BandNormalized(
                Some(UnnormalizedSED.UserDefined(customSed)),
                brightnesses
              )
            ),
            redshift
          )
    case TargetData(
          SourceProfile.Uniform(
            SpectralDefinition.BandNormalized(
              Some(UnnormalizedSED.UserDefinedAttachment(id)),
              brightnesses
            )
          ),
          redshift
        ) =>
      Resolver[F]
        .resolve(id)
        .map: customSed =>
          TargetData(
            SourceProfile.Uniform(
              SpectralDefinition.BandNormalized(
                Some(UnnormalizedSED.UserDefined(customSed)),
                brightnesses
              )
            ),
            redshift
          )
    case TargetData(
          SourceProfile.Gaussian(
            fwhm,
            SpectralDefinition.BandNormalized(
              Some(UnnormalizedSED.UserDefinedAttachment(id)),
              brightnesses
            )
          ),
          redshift
        ) =>
      Resolver[F]
        .resolve(id)
        .map: customSed =>
          TargetData(
            SourceProfile.Gaussian(
              fwhm,
              SpectralDefinition.BandNormalized(
                Some(UnnormalizedSED.UserDefined(customSed)),
                brightnesses
              )
            ),
            redshift
          )
    case other => other.pure[F]

  def resolveTargetGraphRequest[F[_]: Monad: Resolver]
    : TargetGraphRequest => F[TargetGraphRequest] =
    case TargetGraphRequest(targetData, parameters) =>
      resolveTargetData(targetData).map(TargetGraphRequest(_, parameters))

  def resolveTargetSpectroscopyTimeRequest[F[_]: Monad: Resolver]
    : TargetSpectroscopyTimeRequest => F[TargetSpectroscopyTimeRequest] =
    case TargetSpectroscopyTimeRequest(targetData, parameters) =>
      resolveTargetData(targetData).map(TargetSpectroscopyTimeRequest(_, parameters))

  def resolveTargetImagingTimeRequest[F[_]: Monad: Resolver]
    : TargetImagingTimeRequest => F[TargetImagingTimeRequest] =
    case TargetImagingTimeRequest(targetData, parameters) =>
      resolveTargetData(targetData).map(TargetImagingTimeRequest(_, parameters))
