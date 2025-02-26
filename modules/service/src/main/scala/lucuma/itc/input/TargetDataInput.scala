// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.input

import cats.Applicative
import cats.syntax.functor.*
import cats.syntax.parallel.*
import lucuma.core.math.RadialVelocity
import lucuma.core.model.SourceProfile
import lucuma.odb.graphql.binding.*
import lucuma.odb.graphql.input.*
import lucuma.odb.graphql.input.sourceprofile.*

import customSed.CustomSed

case class TargetDataInput(
  sourceProfile:  SourceProfile,
  radialVelocity: RadialVelocity
)

object TargetDataInput {
  def binding[F[_]: Applicative: CustomSed.Resolver]: Matcher[F[TargetDataInput]] =
    ObjectFieldsBinding.rmap {
      case List(
            SourceProfileInput.binding("sourceProfile", sourceProfile),
            RadialVelocityInput.Binding("radialVelocity", radialVelocity)
          ) =>
        (sourceProfile, radialVelocity).parMapN((sp, rv) => sp.map(TargetDataInput(_, rv)))
    }
}
