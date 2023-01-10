// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.syntax.all

import cats.Semigroup
import io.circe.ACursor

// An "orElse" semigroup for ACursor
given Semigroup[ACursor] =
  new Semigroup[ACursor]:
    def combine(a: ACursor, b: ACursor): ACursor =
      if (a.failed) b else a
