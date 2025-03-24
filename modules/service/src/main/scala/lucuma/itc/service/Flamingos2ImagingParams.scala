// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import cats.Eq
import cats.derived.*
import io.circe.Encoder
import lucuma.core.enums.F2Filter

case class Flamingos2ImagingParams(filter: F2Filter) derives Eq, Encoder.AsObject
