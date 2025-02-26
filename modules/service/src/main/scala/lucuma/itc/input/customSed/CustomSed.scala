// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.input.customSed

import cats.data.NonEmptyMap
import eu.timepit.refined.types.numeric.PosBigDecimal
import lucuma.core.math.Wavelength

object CustomSed:
  type Id = String

  trait Resolver[F[_]]:
    def resolve(id: Id): F[NonEmptyMap[Wavelength, PosBigDecimal]]

  object Resolver:
    def apply[F[_]](using r: Resolver[F]): Resolver[F] = r
