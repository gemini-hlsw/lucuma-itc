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
import cats.syntax.all.*
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.UnnormalizedSED
import lucuma.itc.service.requests.TargetGraphRequest
import lucuma.itc.service.requests.TargetSpectroscopyTimeRequest
import lucuma.itc.service.requests.TargetImagingTimeRequest
import cats.Parallel
import cats.Applicative
import cats.Monad

object CustomSed:
  trait Resolver[F[_]]:
    def resolve(id: Attachment.Id): F[NonEmptyMap[Wavelength, PosBigDecimal]]

  object Resolver:
    def apply[F[_]](using r: Resolver[F]): Resolver[F] = r

  private def resolveUnnormalizedSed[F[_]: Applicative: Resolver]
    : UnnormalizedSED => F[UnnormalizedSED] =
    case UnnormalizedSED.UserDefinedAttachment(id) =>
      Resolver[F].resolve(id).map(UnnormalizedSED.UserDefined(_))
    case other                                     => other.pure[F]

  def resolveTargetData[F[_]: Monad: Parallel: Resolver]: TargetData => F[TargetData] =
    TargetData.sourceProfile
      .andThen(SourceProfile.integratedBandNormalizedSpectralDefinition)
      .andThen(BandNormalized.sed.some)
      .parModifyF(resolveUnnormalizedSed)
      .andThen:
        _.flatMap:
          TargetData.sourceProfile
            .andThen(SourceProfile.surfaceBandNormalizedSpectralDefinition)
            .andThen(BandNormalized.sed.some)
            .parModifyF(resolveUnnormalizedSed)

  def resolveTargetGraphRequest[F[_]: Monad: Parallel: Resolver]
    : TargetGraphRequest => F[TargetGraphRequest] =
    case TargetGraphRequest(targetData, parameters) =>
      resolveTargetData(targetData).map(TargetGraphRequest(_, parameters))

  def resolveTargetSpectroscopyTimeRequest[F[_]: Monad: Parallel: Resolver]
    : TargetSpectroscopyTimeRequest => F[TargetSpectroscopyTimeRequest] =
    case TargetSpectroscopyTimeRequest(targetData, parameters) =>
      resolveTargetData(targetData).map(TargetSpectroscopyTimeRequest(_, parameters))

  def resolveTargetImagingTimeRequest[F[_]: Monad: Parallel: Resolver]
    : TargetImagingTimeRequest => F[TargetImagingTimeRequest] =
    case TargetImagingTimeRequest(targetData, parameters) =>
      resolveTargetData(targetData).map(TargetImagingTimeRequest(_, parameters))
