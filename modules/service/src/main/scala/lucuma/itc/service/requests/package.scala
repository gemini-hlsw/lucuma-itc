// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service.requests

import cats.data.NonEmptyChain
import cats.syntax.all.*
import grackle.Result
import lucuma.itc.input.TargetDataInput
import lucuma.itc.service.TargetData

private def targetInputsToData(
  asterism: List[TargetDataInput]
): Result[NonEmptyChain[TargetData]] =
  Result
    .fromOption(NonEmptyChain.fromSeq(asterism), "No targets provided")
    .flatMap:
      _.traverse: targetDataInput =>
        Result
          .fromOption(
            targetDataInput.radialVelocity.toRedshift,
            s"Invalid radial velocity: ${targetDataInput.radialVelocity}"
          )
          .map(z => TargetData(targetDataInput.sourceProfile, z))
