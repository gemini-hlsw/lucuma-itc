// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service.requests

import cats.*
import cats.data.NonEmptyChain
import cats.derived.*
import cats.syntax.all.*
import eu.timepit.refined.*
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.types.numeric.PosInt
import grackle.*
import lucuma.core.enums.{ExecutionEnvironment as _, *}
import lucuma.core.math.Wavelength
import lucuma.core.model.sequence.gmos.GmosFpuMask
import lucuma.core.util.TimeSpan
import lucuma.itc.*
import lucuma.itc.input.*
import lucuma.itc.search.GmosNorthFpuParam
import lucuma.itc.search.GmosSouthFpuParam
import lucuma.itc.search.ObservingMode
import lucuma.itc.search.TargetData
import lucuma.itc.search.hashes.given

case class GraphParameters(
  wavelength:      Wavelength,
  specMode:        ObservingMode.SpectroscopyMode,
  constraints:     ItcObservingConditions,
  expTime:         TimeSpan,
  exp:             PosInt,
  signalToNoiseAt: Option[Wavelength]
) derives Hash

case class TargetGraphRequest(
  target:     TargetData,
  parameters: GraphParameters
) derives Hash:
  export parameters.*

case class AsterismGraphRequest(
  asterism:           NonEmptyChain[TargetData],
  parameters:         GraphParameters,
  significantFigures: Option[SignificantFigures]
) derives Hash:
  export parameters.*

  def toTargetRequests: NonEmptyChain[TargetGraphRequest] =
    asterism.map:
      TargetGraphRequest(_, parameters)

object AsterismGraphRequest:
  def fromInput(input: SpectroscopyGraphsInput): Result[AsterismGraphRequest] = {
    val SpectroscopyGraphsInput(
      wavelength,
      signalToNoiseAt,
      exposureTime,
      exposureCount,
      asterism,
      constraints,
      mode,
      figures
    ) = input

    val asterismResult: Result[NonEmptyChain[TargetData]] =
      targetInputsToData(asterism)

    val modeResult: Result[ObservingMode.SpectroscopyMode] = mode match {
      case GmosNSpectroscopyInput(grating, GmosFpuMask.Builtin(fpu), filter, ccdMode, roi) =>
        Result(
          ObservingMode.SpectroscopyMode
            .GmosNorth(wavelength, grating, GmosNorthFpuParam(fpu), filter, ccdMode, roi)
        )
      case GmosSSpectroscopyInput(grating, GmosFpuMask.Builtin(fpu), filter, ccdMode, roi) =>
        Result(
          ObservingMode.SpectroscopyMode
            .GmosSouth(wavelength, grating, GmosSouthFpuParam(fpu), filter, ccdMode, roi)
        )
      case _                                                                               =>
        Result.failure("Invalid spectroscopy mode")
    }

    val conditionsResult: Result[ItcObservingConditions] =
      constraints.create
        .flatMap(c => Result.fromEither(ItcObservingConditions.fromConstraints(c)))

    (asterismResult, modeResult, conditionsResult).parMapN: (asterism, mode, conditions) =>
      AsterismGraphRequest(
        asterism,
        GraphParameters(
          wavelength,
          mode,
          conditions,
          exposureTime,
          exposureCount,
          signalToNoiseAt
        ),
        figures
      )
  }
