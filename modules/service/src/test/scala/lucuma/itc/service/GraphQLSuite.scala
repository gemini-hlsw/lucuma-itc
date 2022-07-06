// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import cats.effect._
import io.circe.Json
import io.circe.parser._
import lucuma.itc.Itc
import lucuma.itc.ItcObservingConditions
import lucuma.itc.search.ObservingMode
import lucuma.itc.search.TargetProfile
import lucuma.itc.service.config.ExecutionEnvironment
import natchez.Trace.Implicits.noop
import org.http4s._
import org.http4s.circe._
import org.http4s.syntax.all._
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.concurrent.duration._

trait GraphQLSuite extends munit.CatsEffectSuite {
  implicit val unsafeLogger: Logger[IO] = Slf4jLogger.getLogger[IO]

  val itc = new Itc[IO] {
    def calculate(
      targetProfile: TargetProfile,
      observingMode: ObservingMode,
      constraints:   ItcObservingConditions,
      signalToNoise: BigDecimal
    ): IO[Itc.Result] =
      IO.pure(
        Itc.Result.Success(1.seconds, 10, 10)
      )
  }

  val service: IO[HttpRoutes[IO]] =
    ItcMapping[IO](ExecutionEnvironment.Local, itc).map(m =>
      ItcService.routes[IO](ItcService.service[IO](m))
    )

  val itcFixture = ResourceSuiteLocalFixture(
    "itc",
    Resource.make(service)(_ => IO.unit)
  )

  override def munitFixtures = List(itcFixture)

  def query(query: String, expected: Json): IO[Unit] =
    IO(itcFixture())
      .flatMap { itc =>
        itc.orNotFound.run(
          Request(method = Method.POST, uri = uri"/itc")
            .withEntity(Json.obj("query" -> Json.fromString(query)))
        )
      }
      .flatMap(_.as[Json])
      .assertEquals(expected)

  def query(query: String, variables: String, expected: Json): IO[Unit] =
    IO(itcFixture())
      .flatMap { itc =>
        itc.orNotFound.run(
          Request(method = Method.POST, uri = uri"/itc")
            .withEntity(
              Json.obj("query"     -> Json.fromString(query.replace("\\n", "")),
                       "variables" -> parse(variables).getOrElse(Json.Null)
              )
            )
        )
      }
      .flatMap(_.as[Json])
      .assertEquals(expected)
}
