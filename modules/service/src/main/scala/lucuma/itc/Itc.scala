// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import cats.data.NonEmptyChain
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.Band
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.util.TimeSpan
import lucuma.itc.search.*

trait Itc[F[_]]:

  /**
   * Compute the exposure time and number of exposures required to achieve the desired
   * signal-to-noise under the requested conditions.
   */
  def calculateIntegrationTime(
    target:        TargetData,
    atWavelength:  Wavelength,
    observingMode: ObservingMode,
    constraints:   ItcObservingConditions,
    signalToNoise: SignalToNoise
  ): F[TargetIntegrationTime]

  /**
   * Retrieve the graph data for the given mode and exposureTime and exposures
   */
  def calculateGraph(
    target:        TargetData,
    atWavelength:  Wavelength,
    observingMode: ObservingMode,
    constraints:   ItcObservingConditions,
    exposureTime:  TimeSpan,
    exposureCount: PosInt
  ): F[TargetGraphsCalcResult]

object Itc:
  def apply[F[_]](using ev: Itc[F]): ev.type = ev
