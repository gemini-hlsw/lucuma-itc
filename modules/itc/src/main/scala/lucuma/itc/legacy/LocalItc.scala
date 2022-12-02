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
    .loadClass("edu.gemini.itc.web.servlets.ItcCalculationImpl")
    .getMethod("calculation", classOf[String], classOf[String])

  def callLocal(call: String): Either[String, ItcRemoteResult] = {
    val res = method
      .invoke(
        null,
        call,
        "token"
      )
      .asInstanceOf[String]

    val LegacyRight = """Right\((.*)\)""".r
    val LegacyLeft  = """Left\((.*)\)""".r

    res match {
      case LegacyRight(result) =>
        decode[legacy.ItcRemoteResult](result).leftMap { e =>
          println(result)
          e.printStackTrace(); e.getMessage()
        }
      case LegacyLeft(result)  =>
        Left(result)
      case m                   =>
        Left(s"Unknown result: $m")
    }
  }
}
