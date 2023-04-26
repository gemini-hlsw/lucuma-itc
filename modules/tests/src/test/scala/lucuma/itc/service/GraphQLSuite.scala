// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import cats.Applicative
import cats.ApplicativeThrow
import cats.data.NonEmptyList
import cats.effect._
import cats.syntax.all._
import io.circe.Json
import io.circe.parser._
import lucuma.core.math.Wavelength
import lucuma.itc.Itc
import lucuma.itc.tests.FailingMockItc
import lucuma.itc.tests.MockItc
import lucuma.itc.tests.NoOpRedis
import natchez.Trace.Implicits.noop
import org.http4s._
import org.http4s.circe._
import org.http4s.syntax.all._
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import java.time.Duration
import scala.concurrent.duration._

trait GraphQLSuiteBase extends munit.CatsEffectSuite:
  given Logger[IO] = Slf4jLogger.getLogger[IO]

  def itcService: Itc[IO]

  val service = lucuma.itc.tests.routes(itcService)

  val itcFixture = ResourceSuiteLocalFixture(
    "itc",
    Resource.make(service)(_ => IO.unit)
  )

  override def munitFixtures = List(itcFixture)

  def query(query: String, expected: Json): IO[Unit] =
    IO(itcFixture())
      .flatMap { itc =>
        itc.orNotFound.run(
          Request(method = Method.POST, uri = uri"/graphql")
            .withEntity(Json.obj("query" -> Json.fromString(query)))
        )
      }
      .flatMap(_.as[Json])
      .assertEquals(expected)

  def query(query: String, variables: String, expected: Json): IO[Unit] =
    IO(itcFixture())
      .flatMap { itc =>
        itc.orNotFound.run(
          Request(method = Method.POST, uri = uri"/graphql")
            .withEntity(
              Json.obj("query"     -> Json.fromString(query.replace("\\n", "")),
                       "variables" -> parse(variables).getOrElse(Json.Null)
              )
            )
        )
      }
      .flatMap(_.as[Json])
      .assertEquals(expected)

trait GraphQLSuite extends GraphQLSuiteBase:
  override def itcService = MockItc

trait FailingCalculationSuite extends GraphQLSuiteBase:
  override def itcService = FailingMockItc
