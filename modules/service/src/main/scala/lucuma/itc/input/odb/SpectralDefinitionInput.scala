// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input
package sourceprofile

import cats.Applicative
import cats.syntax.all.*
import grackle.Result
import lucuma.core.math.BrightnessUnits.*
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.SpectralDefinition.BandNormalized
import lucuma.core.model.SpectralDefinition.EmissionLines
import lucuma.odb.graphql.binding.*

object SpectralDefinitionInput {

  implicit class SpectralDefinitionProjections[A](self: SpectralDefinition[A]) {
    def bandNormalized = self match
      case a: BandNormalized[A] => Result(a);
      case _                    => Result.failure("Not a band normalized spectral definition.")

    def emissionLines = self match
      case a: EmissionLines[A] => Result(a);
      case _                   => Result.failure("Not a emission lines spectral definition.")
  }

  object Integrated {
    def binding[F[_]: Applicative]: Matcher[F[SpectralDefinition[Integrated]]] =
      bindingInternal[F, Integrated](
        BandNormalizedInput.Integrated.binding,
        EmissionLinesInput.Integrated.Binding
      )
  }

  object Surface {
    def binding[F[_]: Applicative]: Matcher[F[SpectralDefinition[Surface]]] =
      bindingInternal[F, Surface](
        BandNormalizedInput.Surface.binding,
        EmissionLinesInput.Surface.Binding
      )
  }

  private def bindingInternal[F[_]: Applicative, A](
    bandNormalized: Matcher[F[BandNormalized[A]]],
    emissionLines:  Matcher[EmissionLines[A]]
  ): Matcher[F[SpectralDefinition[A]]] =
    ObjectFieldsBinding.rmap {
      case List(
            bandNormalized.Option("bandNormalized", rBandNormalized),
            emissionLines.Option("emissionLines", rEmissionLines)
          ) =>
        (rBandNormalized, rEmissionLines).parTupled.flatMap {
          case (Some(bandNormalized), None) => Result(bandNormalized.widen)
          case (None, Some(emissionLines))  => Result(emissionLines.pure[F])
          case _                            => Result.failure("Expected exactly one of bandNormalized or emissionLines.")
        }
    }
}
