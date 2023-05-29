// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import cats.data.NonEmptyList
import io.circe.*
import lucuma.core.math.SignalToNoise
import lucuma.core.util.NewType
import lucuma.itc.encoders.given

case class ItcVersions(
  serverVersion: String,
  dataVersion:   Option[String]
) derives Encoder.AsObject

object FinalSN extends NewType[SignalToNoise]
type FinalSN = FinalSN.Type

object SingleSN extends NewType[SignalToNoise]
type SingleSN = SingleSN.Type
