// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import lucuma.itc.legacy.*
import lucuma.itc.service.Main.ReverseClassLoader
import munit.Tag

import java.io.File
import java.io.FileFilter

object LocalOnly extends Tag("LocalOnly")

/**
 * This is a common trait for tests of the legacy ITC code
 */
trait CommonITCSuite:

  def allowedErrors(err: List[String]) =
    err.exists(_.contains("Invalid S/N")) || err.exists(_.contains("do not overlap")) ||
      err.exists(_.contains("Unsupported configuration")) ||
      err.exists(_.contains("Unsupported calculation method")) ||
      err.exists(_.contains("target is too bright"))

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
