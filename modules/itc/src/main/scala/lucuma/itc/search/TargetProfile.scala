// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.search

import cats.Hash
import cats.derived.*
import lucuma.core.enums.Band
import lucuma.core.math.Redshift
import lucuma.core.model.SourceProfile
import lucuma.itc.search.hashes.given

/** Target properties we need to know at phase zero. */
final case class TargetProfile(
  sourceProfile: SourceProfile,
  band:          Band,
  redshift:      Redshift
) derives Hash
