// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import cats.Semigroup
import cats.data.NonEmptyList
import cats.implicits._
import io.circe.Decoder
import io.circe.HCursor

case class ItcGraphResult(charts: NonEmptyList[ItcChart])
