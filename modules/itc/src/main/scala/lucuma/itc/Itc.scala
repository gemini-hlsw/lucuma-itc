// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import cats.data.NonEmptyList
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosLong
import io.circe.*
import io.circe.syntax.*
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.model.NonNegDuration
import lucuma.core.util.Enumerated
import lucuma.itc.encoders.given
import lucuma.itc.legacy.ItcRemoteCcd
import lucuma.itc.search.*

import scala.concurrent.duration.FiniteDuration

trait Itc[F[_]]:

  /**
   * Compute the exposure time and number of exposures required to achieve the desired
   * signal-to-noise under the requested conditions.
   */
  def calculateIntegrationTime(
    targetProfile:   TargetProfile,
    observingMode:   ObservingMode,
    constraints:     ItcObservingConditions,
    signalToNoise:   SignalToNoise,
    signalToNoiseAt: Option[Wavelength]
  ): F[IntegrationTime]

  /**
   * Retrieve the graph data for the given mode and exposureTime and exposures
   */
  def calculateGraph(
    targetProfile: TargetProfile,
    observingMode: ObservingMode,
    constraints:   ItcObservingConditions,
    exposureTime:  NonNegDuration,
    exposures:     PosLong
  ): F[GraphResult]

  /**
   * Calculate the signal to noise from graph data for the given mode and exposureTime and amount of
   * exposures at a specific wavelength
   */
  def calculateSignalToNoise(
    graph:           NonEmptyList[ItcChartGroup],
    signalToNoiseAt: Option[Wavelength]
  ): F[SNCalcResult]

object Itc:

  def apply[F[_]](using ev: Itc[F]): ev.type = ev
