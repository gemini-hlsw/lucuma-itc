// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service.requests

import cats.*
import cats.data.NonEmptyChain
import cats.derived.*
import cats.syntax.all.*
import grackle.*
import lucuma.core.enums.{ExecutionEnvironment as _, *}
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.sequence.gmos.GmosFpuMask
import lucuma.itc.*
import lucuma.itc.input.*
import lucuma.itc.search.GmosNorthFpuParam
import lucuma.itc.search.GmosSouthFpuParam
import lucuma.itc.search.ObservingMode
import lucuma.itc.search.TargetData
import lucuma.itc.search.hashes.given

case class SpectroscopyTimeParameters(
  exposureTimeMode: ExposureTimeMode,
  specMode:         ObservingMode.SpectroscopyMode,
  constraints:      ItcObservingConditions
) derives Hash

case class TargetSpectroscopyTimeRequest(
  target:     TargetData,
  parameters: SpectroscopyTimeParameters
) derives Hash:
  export parameters.*

case class AsterismSpectroscopyTimeRequest(
  asterism:   NonEmptyChain[TargetData],
  parameters: SpectroscopyTimeParameters
) derives Hash:
  export parameters.*

  def toTargetRequests: NonEmptyChain[TargetSpectroscopyTimeRequest] =
    asterism.map:
      TargetSpectroscopyTimeRequest(_, parameters)

object AsterismSpectroscopyTimeRequest:
  def fromInput(input: SpectroscopyTimeInput): Result[AsterismSpectroscopyTimeRequest] = {
    val SpectroscopyTimeInput(exposureTimeMode, asterism, constraints, mode) =
      input

    val asterismResult: Result[NonEmptyChain[TargetData]] =
      targetInputsToData(asterism)

    val modeResult: Result[ObservingMode.SpectroscopyMode] =
      mode match
        case GmosNSpectroscopyInput(
              centralWavelength,
              grating,
              GmosFpuMask.Builtin(fpu),
              filter,
              ccdMode,
              roi
            ) =>
          Result.success:
            ObservingMode.SpectroscopyMode
              .GmosNorth(centralWavelength, grating, GmosNorthFpuParam(fpu), filter, ccdMode, roi)
        case GmosSSpectroscopyInput(
              centralWavelength,
              grating,
              GmosFpuMask.Builtin(fpu),
              filter,
              ccdMode,
              roi
            ) =>
          Result.success:
            ObservingMode.SpectroscopyMode
              .GmosSouth(centralWavelength, grating, GmosSouthFpuParam(fpu), filter, ccdMode, roi)
        case F2SpectroscopyInput(
              centralWavelength,
              disperser,
              filter,
              fpu
            ) =>
          Result.success:
            ObservingMode.SpectroscopyMode
              .Flamingos2(centralWavelength, disperser, filter, fpu)
        case _ =>
          Result.failure("Invalid spectroscopy mode")

    val conditionsResult: Result[ItcObservingConditions] =
      constraints.create
        .flatMap(c => Result.fromEither(ItcObservingConditions.fromConstraints(c)))

    (asterismResult, modeResult, conditionsResult).parMapN: (asterism, mode, conditions) =>
      AsterismSpectroscopyTimeRequest(
        asterism,
        SpectroscopyTimeParameters(exposureTimeMode, mode, conditions)
      )
  }
