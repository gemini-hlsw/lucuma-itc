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
import lucuma.itc.*
import lucuma.itc.input.*
import lucuma.itc.search.ObservingMode
import lucuma.itc.search.TargetData
import lucuma.itc.search.hashes.given

case class ImagingTimeParameters(
  wavelength:    Wavelength,
  imagingMode:   ObservingMode.ImagingMode,
  constraints:   ItcObservingConditions,
  signalToNoise: SignalToNoise
) derives Hash

case class TargetImagingTimeRequest(
  target:     TargetData,
  parameters: ImagingTimeParameters
) derives Hash:
  export parameters.*

case class AsterismImagingTimeRequest(
  asterism:   NonEmptyChain[TargetData],
  parameters: ImagingTimeParameters
) derives Hash:
  export parameters.*

  def toTargetRequests: NonEmptyChain[TargetImagingTimeRequest] =
    asterism.map:
      TargetImagingTimeRequest(_, parameters)

object AsterismImagingTimeRequest:
  def fromInput(input: ImagingIntegrationTimeInput): Result[AsterismImagingTimeRequest] = {
    val ImagingIntegrationTimeInput(
      wavelength,
      signalToNoise,
      asterism,
      constraints,
      mode
    ) = input

    val asterismResult: Result[NonEmptyChain[TargetData]] =
      targetInputsToData(asterism)

    val modeResult: Result[ObservingMode.ImagingMode] =
      mode match
        case GmosNImagingInput(filter, ccdMode) =>
          Result.success:
            ObservingMode.ImagingMode.GmosNorth(wavelength, filter, ccdMode)
        case GmosSImagingInput(filter, ccdMode) =>
          Result.success:
            ObservingMode.ImagingMode.GmosSouth(wavelength, filter, ccdMode)
        case _                                  =>
          Result.failure("Invalid imaging mode")

    val conditionsResult: Result[ItcObservingConditions] =
      constraints.create
        .flatMap(c => Result.fromEither(ItcObservingConditions.fromConstraints(c)))

    (asterismResult, modeResult, conditionsResult).parMapN: (asterism, mode, conditions) =>
      AsterismImagingTimeRequest(
        asterism,
        ImagingTimeParameters(wavelength, mode, conditions, signalToNoise)
      )
  }
