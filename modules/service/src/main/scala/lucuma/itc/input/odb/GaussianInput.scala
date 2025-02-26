// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input
package sourceprofile

import cats.Applicative
import cats.syntax.all.*
import grackle.Result
import lucuma.core.model.SourceProfile
import lucuma.odb.graphql.binding.*
import lucuma.itc.input.CustomSed

object GaussianInput {

  def binding[F[_]: Applicative: CustomSed.Resolver]: Matcher[F[SourceProfile.Gaussian]] =
    ObjectFieldsBinding.rmap {
      case List(
            AngleInput.Binding.Option("fwhm", rFwhm),
            SpectralDefinitionInput.Integrated.binding
              .Option("spectralDefinition", rSpectralDefinition)
          ) =>
        (rFwhm, rSpectralDefinition).parTupled.flatMap {
          case (Some(fwhm), Some(spectralDefinition)) =>
            Result(spectralDefinition.map(SourceProfile.Gaussian(fwhm, _)))
          case _                                      =>
            Result.failure("Both fwhm and spectralDefinition must be provided on creation")
        }
    }
}
