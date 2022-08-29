// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.search.hashes

import cats.Hash
import cats.implicits.*
import lucuma.core.math.Redshift
import lucuma.core.enums.Band
import lucuma.core.model.SourceProfile
import lucuma.core.util.Enumerated
import eu.timepit.refined._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.api.Validate
import java.time.Duration
import lucuma.core.model.NonNegDuration
import lucuma.core.math.Wavelength

given hastEnumerated[A: Enumerated]: Hash[A] = Hash.by(summon[Enumerated[A]].tag)

given hashRefined[A: Hash, B](using Validate[A, B]): Hash[A Refined B] =
  Hash.by(_.value)

given Hash[Redshift]       = Hash.by(_.z)
given Hash[Duration]       = Hash.by(_.getNano())
given Hash[NonNegDuration] = Hash.by(_.value)
given Hash[SourceProfile]  = Hash.fromUniversalHashCode[SourceProfile]
given Hash[Wavelength]     = Hash.by(_.toPicometers.value)
