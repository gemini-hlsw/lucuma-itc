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
import lucuma.odb.graphql.binding.*

import scala.collection.immutable.SortedMap

object BandNormalizedInput {

  /** Binding[(K, V)] to Binding[SortedMap[K, V]] */
  def pairToMap[K: Ordering, V](pair: Matcher[(K, V)]): Matcher[SortedMap[K, V]] =
    pair.List.map(SortedMap.from(_))

  object Integrated {

    def createBinding[F[_]: Applicative]: Matcher[F[BandNormalized[Integrated]]] =
      createBindingInternal(pairToMap(BandBrightnessInput.Integrated.Binding))

    // val EditBinding: Matcher[BandNormalized[Integrated] => Result[BandNormalized[Integrated]]] =
    //   editBinding(pairToMap(BandBrightnessInput.Integrated.Binding))

  }

  object Surface {

    def createBinding[F[_]: Applicative]: Matcher[F[BandNormalized[Surface]]] =
      createBindingInternal(pairToMap(BandBrightnessInput.Surface.Binding))

    // def editBinding[F[_]: Applicative]: Matcher[F[BandNormalized[Surface]] => Result[F[BandNormalized[Surface]]] =
    //   editBinding(pairToMap(BandBrightnessInput.Surface.Binding))
  }

  private def createBindingInternal[F[_]: Applicative, A](
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
          case _                         => Result.failure("Brightness is required.")
        }
    }

  // def editBinding[F[_]: Applicative,A](
  //   brightnesses: Matcher[SortedMap[Band, BrightnessMeasure[A]]]
  // ): Matcher[BandNormalized[A] => Result[BandNormalized[A]]] =
  //   ObjectFieldsBinding.rmap {
  //     case List(
  //           UnnormalizedSedInput.binding.Nullable("sed", rSed),
  //           brightnesses.Option("brightnesses", rBrightnesses)
  //         ) =>
  //       (rSed.sequence, rBrightnesses).parTupled.flatMap { case (sed, brightnesses) =>
  //         Result { a0 =>
  //           val a1 = sed.fold(a0.copy(sed = none), a0, b => a0.copy(sed = b.some))
  //           val a2 = brightnesses.foldLeft(a1)((a, b) => a.copy(brightnesses = b))
  //           Result(a2)
  //         }
  //       }
  //   }
}
