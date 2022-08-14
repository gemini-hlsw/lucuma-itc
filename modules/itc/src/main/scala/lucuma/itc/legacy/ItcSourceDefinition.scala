// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.legacy

import io.circe.Encoder
import io.circe.Json
import io.circe.syntax._
import lucuma.core.enums._
import lucuma.core.math.Redshift
import lucuma.core.model.SourceProfile
import lucuma.itc.search.TargetProfile

final case class ItcSourceDefinition(
  profile:  SourceProfile,
  normBand: Band,
  redshift: Redshift
)

object ItcSourceDefinition {

  def fromTargetProfile(p: TargetProfile): ItcSourceDefinition =
    ItcSourceDefinition(
      p.sourceProfile,
      p.band,
      p.redshift
    )
}
