// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service.config

import cats.implicits._
import ciris._

sealed trait ExecutionEnvironment
object ExecutionEnvironment:

  case object Local      extends ExecutionEnvironment
  case object Review     extends ExecutionEnvironment
  case object Staging    extends ExecutionEnvironment
  case object Production extends ExecutionEnvironment

  given ConfigDecoder[String, ExecutionEnvironment] =
    ConfigDecoder[String].map(_.toLowerCase).collect("Environment") {
      case "local"      => Local
      case "review"     => Review
      case "staging"    => Staging
      case "production" => Production
    }
