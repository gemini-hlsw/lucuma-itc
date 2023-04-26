// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import cats.Eq
import cats.derived.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.math.SignalToNoise
import lucuma.core.util.TimeSpan

import java.math.MathContext

case class IntegrationTime(
  exposureTime:  TimeSpan,
  exposures:     PosInt,
  signalToNoise: SignalToNoise
) derives Eq

extension (signalToNoise: SignalToNoise)
  def stepSignalToNoise(exposures: PosInt): Option[SignalToNoise] =
    SignalToNoise.FromBigDecimalRounding.getOption(
      BigDecimal(
        (signalToNoise.toBigDecimal * signalToNoise.toBigDecimal / exposures.value)
          .underlying()
          .sqrt(MathContext.DECIMAL128)
      )
    )
