// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import cats.data.NonEmptyList
import cats.effect._
import cats.syntax.all._
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.numeric.PosLong
import io.circe.Json
import io.circe.parser._
import lucuma.core.model.NonNegDuration
import lucuma.graphql.routes.GrackleGraphQLService
import lucuma.graphql.routes.Routes
import lucuma.itc.ChartType
import lucuma.itc.Itc
import lucuma.itc.ItcCcd
import lucuma.itc.ItcChart
import lucuma.itc.ItcChart.apply
import lucuma.itc.ItcChartGroup
import lucuma.itc.ItcObservingConditions
import lucuma.itc.ItcSeries
import lucuma.itc.SeriesDataType
import lucuma.itc.search.ObservingMode
import lucuma.itc.search.TargetProfile
import lucuma.itc.service.config.ExecutionEnvironment
import natchez.Trace.Implicits.noop
import org.http4s._
import org.http4s.circe._
import org.http4s.server.websocket.WebSocketBuilder2
import org.http4s.syntax.all._
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import java.time.Duration
import scala.concurrent.duration._

trait GraphQLSuite extends munit.CatsEffectSuite:
  given Logger[IO] = Slf4jLogger.getLogger[IO]

  val itc = new Itc[IO]:
    def calculate(
      targetProfile: TargetProfile,
      observingMode: ObservingMode,
      constraints:   ItcObservingConditions,
      signalToNoise: BigDecimal
    ): IO[(Option[String], Itc.Result)] =
      (none, Itc.Result.Success(1.seconds, 10, 10)).pure[IO]

    def calculateGraph(
      targetProfile: TargetProfile,
      observingMode: ObservingMode,
      constraints:   ItcObservingConditions,
      exposureTime:  NonNegDuration,
      exposures:     PosLong
    ): IO[Itc.GraphResult] =
      Itc
        .GraphResult(
          "1",
          NonEmptyList.of(
            ItcCcd(1, 2, 3, 4, 5, Nil)
          ),
          NonEmptyList.of(
            ItcChartGroup(
              NonEmptyList.of(
                ItcChart(
                  ChartType.S2NChart,
                  List(
                    ItcSeries("title",
                              SeriesDataType.BackgroundData,
                              List((1.0, 1000.0), (2.0, 1001.0))
                    )
                  )
                )
              )
            )
          )
        )
        .pure[IO]

    override def itcVersions = IO.pure("versionToken")

  val service: IO[HttpRoutes[IO]] =
    for
      wsb <- WebSocketBuilder2[IO]
      map <- ItcMapping[IO](ExecutionEnvironment.Local, itc)
    yield Routes.forService(_ => IO.pure(GrackleGraphQLService(map).some), wsb)

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
