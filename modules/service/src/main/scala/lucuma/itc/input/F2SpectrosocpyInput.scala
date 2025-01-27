// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.input

import cats.syntax.parallel.*
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.odb.graphql.binding.*
import lucuma.odb.graphql.input.GmosCcdModeInput
import lucuma.core.enums.F2Disperser
import lucuma.core.model.sequence.f2.F2DynamicConfig
import lucuma.core.enums.F2Fpu
import lucuma.odb.graphql.binding.*
import lucuma.odb.graphql.input.*
import lucuma.core.enums.F2Filter
import lucuma.core.math.Wavelength

case class F2SpectroscopyInput(
  centralWavelength: Wavelength,
  disperser:         F2Disperser,
  filter:            F2Filter,
  fpu:               F2Fpu
) extends InstrumentModesInput

object F2SpectroscopyInput:

  def binding: Matcher[F2SpectroscopyInput] =
    ObjectFieldsBinding.rmap {
      case List(
            WavelengthInput.Binding("centralWavelength", centralWavelength),
            F2DisperserBinding("disperser", disperser),
            // F2FpuInput.Binding("fpu", fpu),
            F2FilterBinding("filter", filter),
            F2FpuBinding("fpu", fpu)
          ) =>
        (centralWavelength, disperser, filter, fpu).parMapN(apply)
    }
