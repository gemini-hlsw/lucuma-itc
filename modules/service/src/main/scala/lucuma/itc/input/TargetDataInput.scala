// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.input

import cats.syntax.parallel.*
import lucuma.core.math.RadialVelocity
import lucuma.core.model.SourceProfile
import lucuma.odb.graphql.binding.*
import lucuma.odb.graphql.input.*
import lucuma.odb.graphql.input.sourceprofile.*

case class TargetDataInput(
  sourceProfile:  SourceProfile,
  radialVelocity: RadialVelocity
)

object TargetDataInput {
  def binding: Matcher[TargetDataInput] =
    ObjectFieldsBinding.rmap {
      case List(
            SourceProfileInput.CreateBinding("sourceProfile", sourceProfile),
            RadialVelocityInput.Binding("radialVelocity", radialVelocity)
          ) =>
        (sourceProfile, radialVelocity).parMapN(apply)
    }
}
