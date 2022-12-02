// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.legacy

import cats.syntax.all.*
import io.circe.parser.decode
import lucuma.itc.legacy
import lucuma.itc.legacy.given

case class LocalItc(classLoader: ClassLoader) {
  // We need to keep a single refernce to the reflected method
  val method = classLoader
    .loadClass("edu.gemini.itc.web.servlets.ItcCalculation")
    .getMethod("calculation", classOf[String], classOf[String])

  /**
   * This method does a call to the method ItcCalculation.calculation via reflection. This is done
   * because the itc-server runs on scala 3 while the ItcCalculation method is based on scala 2
   *
   * Doing the call via reflection with a custom class loader lets us have both scala sversion
   * playing in harmony.
   *
   * Note that the param is passed as a String for the same reason avoiding conflicts across classes
   * that may not be compatible. Instead we pass back and forth json encoded version of the params
   * essentially the same as if ITC were a server accepting json and responding json
   */
  def callLocal(call: String): Either[String, ItcRemoteResult] = {
    val res = method
      .invoke(null, call, "token") // null as it is a static method
      .asInstanceOf[String]

    val LegacyRight = """Right\((.*)\)""".r
    val LegacyLeft  = """Left\((.*)\)""".r

    res match {
      case LegacyRight(result) =>
        decode[legacy.ItcRemoteResult](result).leftMap { e =>
          e.getMessage()
        }
      case LegacyLeft(result)  =>
        Left(result)
      case m                   =>
        Left(s"Unknown result: $m")
    }
  }
}
