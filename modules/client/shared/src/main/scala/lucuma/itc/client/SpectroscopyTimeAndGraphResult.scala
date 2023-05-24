// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client

import cats.data.NonEmptyList
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Encoder
import lucuma.core.math.SignalToNoise
import lucuma.core.util.TimeSpan
import lucuma.itc.*
import lucuma.itc.encoders.given

case class SpectroscopyIntegrationTimeAndGraphResult(
  serverVersion:       String,
  dataVersion:         String,
  exposureTime:        TimeSpan,
  exposures:           PosInt,
  ccds:                NonEmptyList[ItcCcd],
  charts:              NonEmptyList[OptimizedChartResult],
  peakSNRatio:         SignalToNoise,
  atWavelengthSNRatio: Option[SignalToNoise]
) derives Encoder.AsObject
