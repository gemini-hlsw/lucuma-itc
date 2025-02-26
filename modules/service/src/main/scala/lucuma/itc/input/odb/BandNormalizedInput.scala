// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input
package sourceprofile

import cats.Applicative
import cats.syntax.all.*
import grackle.Result
import lucuma.core.enums.Band
import lucuma.core.math.BrightnessUnits.*
import lucuma.core.model.SpectralDefinition.BandNormalized
import lucuma.itc.input.customSed.CustomSed
import lucuma.odb.graphql.binding.*

import scala.collection.immutable.SortedMap

object BandNormalizedInput {

  /** Binding[(K, V)] to Binding[SortedMap[K, V]] */
  def pairToMap[K: Ordering, V](pair: Matcher[(K, V)]): Matcher[SortedMap[K, V]] =
    pair.List.map(SortedMap.from(_))

  object Integrated {
    def binding[F[_]: Applicative: CustomSed.Resolver]: Matcher[F[BandNormalized[Integrated]]] =
      bindingInternal(pairToMap(BandBrightnessInput.Integrated.Binding))
  }

  object Surface {
    def binding[F[_]: Applicative: CustomSed.Resolver]: Matcher[F[BandNormalized[Surface]]] =
      bindingInternal(pairToMap(BandBrightnessInput.Surface.Binding))
  }

  private def bindingInternal[F[_]: Applicative: CustomSed.Resolver, A](
    brightnesses: Matcher[SortedMap[Band, BrightnessMeasure[A]]]
  ): Matcher[F[BandNormalized[A]]] =
    ObjectFieldsBinding.rmap {
      case List(
            UnnormalizedSedInput.binding.Option("sed", rSed),
            brightnesses.Option("brightnesses", rBrightnesses)
          ) =>
        (rSed, rBrightnesses).parTupled.flatMap {
          case (sed, Some(brightnesses)) =>
            Result(sed.sequence.map(BandNormalized(_, brightnesses)))
          case _                         =>
            Result.failure("Brightness is required.")
        }
    }
}
