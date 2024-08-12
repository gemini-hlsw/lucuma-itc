// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.search

import cats.Hash
import cats.derived.*
import lucuma.core.enums.Band
import lucuma.core.math.Redshift
import lucuma.core.math.Wavelength
import lucuma.core.model.SourceProfile
import lucuma.itc.search.hashes.given

case class TargetData(
  sourceProfile: SourceProfile,
  redshift:      Redshift
) derives Hash:
  def bandFor(wavelength: Wavelength): Band =
    sourceProfile
      .nearestBand(wavelength)
      .map(_._1)
      .getOrElse(throw new RuntimeException("No brightness measures provided for target."))
