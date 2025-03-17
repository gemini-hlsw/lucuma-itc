// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.legacy

import lucuma.itc.service.Main.ReverseClassLoader
import munit.Tag

import java.io.File
import java.io.FileFilter

object LegacyITCTest extends Tag("LegacyItcTest")

/**
 * This is a common trait for tests of the legacy ITC code
 */
trait CommonITCLegacySuite:
  def containsValidResults(r: IntegrationTimeRemoteResult): Boolean =
    r.exposureCalculation.selectedIndex < r.exposureCalculation.exposures.length &&
      r.exposureCalculation.exposures.forall(e => e.exposureTime >= 0 && e.exposureCount.value >= 0)

  def allowedErrors(err: List[String]) =
    err.exists(_.contains("Invalid S/N")) || err.exists(_.contains("do not overlap")) ||
      err.exists(_.contains("Unsupported configuration")) ||
      err.exists(_.contains("Unsupported calculation method")) ||
      err.exists(_.contains("target is too bright")) ||
      err.exists(_.contains("Signal = 0")) ||
      err.exists(_.contains("Redshifted SED")) ||
      err.exists(_.contains("Wavelength"))

  def allowedErrorsWithLargeSN(err: List[String]) =
    err.exists(_.contains("Invalid S/N")) || err.exists(_.contains("do not overlap")) ||
      err.exists(_.contains("Unsupported configuration")) ||
      err.exists(_.contains("Unsupported calculation method")) ||
      err.exists(_.contains("target is too bright")) ||
      err.exists(_.contains("Invalid SignalToNoise value"))

  lazy val localItc = {

    val jarFiles =
      new File("modules/service/ocslib")
        .getAbsoluteFile()
        .listFiles(new FileFilter() {
          override def accept(file: File): Boolean =
            file.getName().endsWith(".jar")
        })
    LocalItc(
      new ReverseClassLoader(jarFiles.map(_.toURI.toURL), ClassLoader.getSystemClassLoader())
    )
  }
