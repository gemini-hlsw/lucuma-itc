// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.search.syntax

import eu.timepit.refined.auto._
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enum.GmosSouthDisperser
import lucuma.core.enum.GmosSouthDisperser._
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.itc.search.Coverage
import spire.math.Rational

/**
 * Syntax extensions for missing properties. These need to be folded back into the lucuma.core
 * enumerations.
 */
final class GmosSouthDisperserOps(val self: GmosSouthDisperser) extends AnyVal {

  /**
   * Reference wavelength (nm) and resolution for 0.5" slit.
   * @see
   *   http://www.gemini.edu/sciops/instruments/gmos/spectroscopy-overview/gratings
   */
  private def reference: (PosInt, PosInt) =
    self match {
      case B1200_G5321 => (463, 3744)
      case R831_G5322  => (757, 4396)
      case B600_G5323  => (461, 1688)
      case R600_G5324  => (926, 3744)
      case B480_G5327  => (422, 1520)
      case R400_G5325  => (764, 1918)
      case R150_G5326  => (717, 631)
    }

  /**
   * Δλ for 0.5" slit.
   * @see
   *   http://hyperphysics.phy-astr.gsu.edu/hbase/phyopt/gratres.html
   */
  private def Δλ: Rational = {
    val (λ, r) = reference
    Rational(λ.value.toLong, r.value.toLong) // r = λ / Δλ
  }

  /** Resolution at λ with the specified slit width (arcsec). */
  def resolution(λ: Wavelength, slitWidth: Angle): Rational =
    (λ.nanometer.value / Δλ) * (Rational(1, 2) / Angle.signedDecimalArcseconds.get(slitWidth))

  /**
   * Simultaneous coverage with Hamamatsu detectors.
   * @see
   *   http://www.gemini.edu/sciops/instruments/gmos/spectroscopy-overview/gratings
   */
  def simultaneousCoverage: Wavelength =
    self match {
      case B1200_G5321 => Wavelength.fromNanometers(159).get
      case R831_G5322  => Wavelength.fromNanometers(230).get
      case B600_G5323  => Wavelength.fromNanometers(307).get
      case R600_G5324  => Wavelength.fromNanometers(318).get
      case R400_G5325  => Wavelength.fromNanometers(462).get
      case B480_G5327  => Wavelength(390000)
      case R150_G5326  => Wavelength.fromNanometers(1190).get
    }

  /** Compute the coverage of this disperser, given a central wavelength. */
  def coverage(λ: Wavelength): Coverage =
    Coverage.centered(λ, simultaneousCoverage)

  /**
   * Dispersion (pm/pixel) with Hamamatsu detectors.
   * @see
   *   http://www.gemini.edu/sciops/instruments/gmos/spectroscopy-overview/gratings
   */
  def dispersion: PosInt =
    self match {
      case B1200_G5321 => 26
      case R831_G5322  => 38
      case B600_G5323  => 50
      case R600_G5324  => 52
      case R400_G5325  => 74
      case B480_G5327  => 62
      case R150_G5326  => 193
    }

  // pedantic: tags are the same in OCS2 and OCS3 but this is just a coincidence
  def ocs2Tag: String =
    self match {
      case B1200_G5321 => "B1200_G5321"
      case R831_G5322  => "R831_G5322"
      case B600_G5323  => "B600_G5323"
      case R600_G5324  => "R600_G5324"
      case B480_G5327  => "B480_G5327"
      case R400_G5325  => "R400_G5325"
      case R150_G5326  => "R150_G5326"
    }
}

trait ToGmosSouthDisperserOps {
  implicit def toGmosSouthDisperserOps(self: GmosSouthDisperser): GmosSouthDisperserOps =
    new GmosSouthDisperserOps(self)
}

object gmossouthdisperser extends ToGmosSouthDisperserOps
