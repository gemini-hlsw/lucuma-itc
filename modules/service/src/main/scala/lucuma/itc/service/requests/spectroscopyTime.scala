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
import lucuma.core.model.sequence.gmos.GmosFpuMask
import lucuma.itc.*
import lucuma.itc.input.*
import lucuma.itc.search.GmosNorthFpuParam
import lucuma.itc.search.GmosSouthFpuParam
import lucuma.itc.search.ObservingMode
import lucuma.itc.search.TargetData
import lucuma.itc.search.hashes.given

case class SpectroscopyTimeParameters(
  wavelength:    Wavelength,
  specMode:      ObservingMode.SpectroscopyMode,
  constraints:   ItcObservingConditions,
  signalToNoise: SignalToNoise
  // signalToNoiseAt: Option[Wavelength]
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
    val SpectroscopyTimeInput(wavelength, signalToNoise, asterism, constraints, mode) =
      input

    val asterismResult: Result[NonEmptyChain[TargetData]] =
      targetInputsToData(asterism)

    val modeResult: Result[ObservingMode.SpectroscopyMode] =
      mode match
        case GmosNSpectroscopyInput(grating, GmosFpuMask.Builtin(fpu), filter, ccdMode, roi) =>
          Result.success:
            ObservingMode.SpectroscopyMode
              .GmosNorth(wavelength, grating, GmosNorthFpuParam(fpu), filter, ccdMode, roi)
        case GmosSSpectroscopyInput(grating, GmosFpuMask.Builtin(fpu), filter, ccdMode, roi) =>
          Result.success:
            ObservingMode.SpectroscopyMode
              .GmosSouth(wavelength, grating, GmosSouthFpuParam(fpu), filter, ccdMode, roi)
        case _                                                                               =>
          Result.failure("Invalid spectroscopy mode")

    val conditionsResult: Result[ItcObservingConditions] =
      constraints.create
        .flatMap(c => Result.fromEither(ItcObservingConditions.fromConstraints(c)))

    (asterismResult, modeResult, conditionsResult).parMapN: (asterism, mode, conditions) =>
      AsterismSpectroscopyTimeRequest(
        asterism,
        SpectroscopyTimeParameters(wavelength, mode, conditions, signalToNoise)
      )
  }
