// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.input

import cats.syntax.parallel.*
import lucuma.core.enums.F2Disperser
import lucuma.core.enums.F2Filter
import lucuma.core.enums.F2Fpu
import lucuma.core.math.Wavelength
import lucuma.odb.graphql.binding.*
import lucuma.odb.graphql.input.*

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
            F2FpuBinding("fpu", fpu),
            F2FilterBinding("filter", filter)
          ) =>
        (centralWavelength, disperser, filter, fpu).parMapN((c, d, f, u) =>
          F2SpectroscopyInput(c, d, f, u)
        )
    }
