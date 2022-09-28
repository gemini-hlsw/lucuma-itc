// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import cats.Applicative
import cats.ApplicativeThrow
import cats.data.NonEmptyList
import cats.effect._
import cats.syntax.all._
import dev.profunktor.redis4cats.algebra.StringCommands
import dev.profunktor.redis4cats.effects.SetArgs
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.numeric.PosLong
import io.circe.Json
import io.circe.parser._
import io.lettuce.core.RedisFuture
import io.lettuce.core.cluster.api.async.RedisClusterAsyncCommands
import lucuma.core.math.Wavelength
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
import lucuma.itc.SignalToNoiseCalculation
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

class NoOpRedis[F[_]: ApplicativeThrow, K, V] extends StringCommands[F, K, V] {

  override def unsafe[A](f: RedisClusterAsyncCommands[K, V] => RedisFuture[A]): F[A] =
    ApplicativeThrow[F].raiseError(new RuntimeException("unsuppported"))

  override def getSet(key: K, value: V): F[Option[V]] = none.pure[F]

  override def get(key: K): F[Option[V]] = none.pure[F]

  override def mSetNx(keyValues: Map[K, V]): F[Boolean] = true.pure[F]

  override def getRange(key: K, start: Long, end: Long): F[Option[V]] = none.pure[F]

  override def mGet(keys: Set[K]): F[Map[K, V]] = Map.empty.pure[F]

  override def unsafeSync[A](f: RedisClusterAsyncCommands[K, V] => A): F[A] =
    ApplicativeThrow[F].raiseError(new RuntimeException("unsuppported"))

  override def incr(key: K): F[Long] = 1L.pure[F]

  override def decrBy(key: K, amount: Long): F[Long] = (amount - 1).pure[F]

  override def strLen(key: K): F[Option[Long]] = none.pure[F]

  override def mSet(keyValues: Map[K, V]): F[Unit] = Applicative[F].unit

  override def decr(key: K): F[Long] = 1L.pure[F]

  override def incrByFloat(key: K, amount: Double): F[Double] = (amount + 1).pure[F]

  override def set(key: K, value: V, setArgs: SetArgs): F[Boolean] = true.pure[F]

  override def set(key: K, value: V): F[Unit] = Applicative[F].unit

  override def setNx(key: K, value: V): F[Boolean] = true.pure[F]

  override def setRange(key: K, value: V, offset: Long): F[Unit] = Applicative[F].unit

  override def setEx(key: K, value: V, expiresIn: FiniteDuration): F[Unit] = Applicative[F].unit

  override def incrBy(key: K, amount: Long): F[Long] = (amount + 1).pure[F]

  override def append(key: K, value: V): F[Unit] = Applicative[F].unit

}
trait GraphQLSuite extends munit.CatsEffectSuite:
  given Logger[IO] = Slf4jLogger.getLogger[IO]

  val itc = new Itc[IO] with SignalToNoiseCalculation[IO]:
    def calculate(
      targetProfile: TargetProfile,
      observingMode: ObservingMode,
      constraints:   ItcObservingConditions,
      signalToNoise: BigDecimal
    ): IO[Itc.CalcResultWithVersion] =
      Itc.CalcResultWithVersion(Itc.CalcResult.Success(1.seconds, 10, 10)).pure[IO]

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
                              SeriesDataType.FinalS2NData,
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
      map <- ItcMapping[IO](ExecutionEnvironment.Local,
                            new NoOpRedis[IO, Array[Byte], Array[Byte]](),
                            itc
             )
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
