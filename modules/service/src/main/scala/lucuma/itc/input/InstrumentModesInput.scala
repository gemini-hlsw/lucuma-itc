// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.input

import cats.syntax.parallel.*
import lucuma.odb.graphql.binding.*
import lucuma.odb.graphql.input.*

trait InstrumentModesInput

object InstrumentModesInput {

  def binding: Matcher[InstrumentModesInput] =
    ObjectFieldsBinding.rmap {
      case List(
            GmosNSpectroscopyInput.binding.Option("gmosNSpectroscopy", gmosNSpectroscopy),
            GmosSSpectroscopyInput.binding.Option("gmosSSpectroscopy", gmosSSpectroscopy),
            GmosNImagingInput.binding.Option("gmosNImaging", gmosNImaging),
            GmosSImagingInput.binding.Option("gmosSImaging", gmosSImaging)
          ) =>
        (gmosNSpectroscopy, gmosSSpectroscopy, gmosNImaging, gmosSImaging).parTupled.flatMap {
          case (gmosNSpectroscopy, gmosSSpectroscopy, gmosNImaging, gmosSImaging) =>
            oneOrFail(gmosNSpectroscopy -> "gmosNSpectroscopy",
                      gmosSSpectroscopy -> "gmosSSpectroscopy",
                      gmosNImaging      -> "gmosNImaging",
                      gmosSImaging      -> "gmosSImaging"
            )
        }
    }

}
