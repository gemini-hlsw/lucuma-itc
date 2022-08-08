// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import cats.Semigroup
import cats.data.NonEmptyList
import cats.implicits._
import io.circe.ACursor
import io.circe.Decoder
import io.circe.HCursor

final case class ItcGraphResult(charts: NonEmptyList[ItcChart])

object ItcGraphResult:

  // An "orElse" semigroup for ACursor
  given Semigroup[ACursor] =
    new Semigroup[ACursor] {
      def combine(a: ACursor, b: ACursor): ACursor =
        if (a.failed) b else a
    }

  given Decoder[ItcGraphResult] = (c: HCursor) =>
    (c.downField("ItcSpectroscopyResult") |+| c.downField("ItcImagingResult"))
      .downField("chartGroups")
      .as[NonEmptyList[ItcChart]]
      .map(ItcGraphResult(_))
