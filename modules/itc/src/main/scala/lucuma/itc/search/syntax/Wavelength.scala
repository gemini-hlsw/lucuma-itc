// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.search.syntax

// import coulomb.refined._
import coulomb.policy.spire.standard.given
import eu.timepit.refined._
import eu.timepit.refined.numeric.Positive
import lucuma.core.math.Wavelength
import spire.std.int._
import lucuma.core.math.refined.*

final class WavelengthOps(val self: Wavelength) extends AnyVal {

  /** Returns the difference of this wavelength and `other`, clipped at Wavelength.Min. */
  def -(other: Wavelength): Wavelength =
    Wavelength.fromPicometers
      .getOption(self.toPicometers.value.value - other.toPicometers.value.value)
      .getOrElse(Wavelength.Min)

  /** Returns the sum of this wavelength and `other`, clipped at Wavelength.Max. */
  def +(other: Wavelength): Wavelength =
    Some(self.toPicometers.value.value + other.toPicometers.value.value)
      .filter(_ <= Wavelength.Max.toPicometers.value.value)
      .flatMap(w => refineV[Positive](w).map(Wavelength.apply).toOption)
      .getOrElse(Wavelength.Max)

}

trait ToWavelengthOps {
  implicit def toWavelengthOps(self: Wavelength): WavelengthOps =
    new WavelengthOps(self)
}

object wavelength extends ToWavelengthOps
