// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.search.syntax

// import coulomb.refined._
import coulomb.policy.spire.standard.given
import eu.timepit.refined._
import eu.timepit.refined.numeric.Positive
import lucuma.core.math.Wavelength
import spire.std.int._

trait WavelengthSyntax:
  extension(self: Wavelength)
    /** Returns the difference of this wavelength and `other`, clipped at Wavelength.Min. */
    def -(other: Wavelength): Wavelength =
      Wavelength.fromPicometers
        .getOption(self.toPicometers.value.value - other.toPicometers.value.value)
        .getOrElse(Wavelength.Min)

    /** Returns the sum of this wavelength and `other`, clipped at Wavelength.Max. */
    def +(other: Wavelength): Wavelength =
      // This works because integer addition "overflows" to negative values
      Wavelength.fromPicometers
        .getOption(self.toPicometers.value.value + self.toPicometers.value.value)
        .getOrElse(Wavelength.Max)

object wavelength extends WavelengthSyntax
