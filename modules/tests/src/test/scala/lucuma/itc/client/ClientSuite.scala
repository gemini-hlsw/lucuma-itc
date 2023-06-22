// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client

import cats.effect.*
import cats.implicits.*
import com.comcast.ip4s.*
import lucuma.itc.tests.MockItc
import munit.CatsEffectSuite
import natchez.Trace.Implicits.noop
import org.http4s.*
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.jdkhttpclient.JdkHttpClient
import org.http4s.server.Server
import org.http4s.server.websocket.WebSocketBuilder2
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.concurrent.duration.*

trait ClientSuite extends CatsEffectSuite {

  given Logger[IO] =
    Slf4jLogger.getLoggerFromClass(getClass)

  private val httpApp: Resource[IO, WebSocketBuilder2[IO] => HttpApp[IO]] =
    Resource.eval(lucuma.itc.tests.app(MockItc))

  private val server: Resource[IO, Server] =
    httpApp.flatMap { app =>
      EmberServerBuilder
        .default[IO]
        .withHost(ipv4"0.0.0.0")
        .withHttpWebSocketApp(app)
        .withShutdownTimeout(2.seconds)
        .build
    }

  private val serverFixture: Fixture[Server] =
    ResourceSuiteLocalFixture("server", server)

  override def munitFixtures = List(serverFixture)

  private val itcClient: IO[ItcClient[IO]] =
    for {
      h <- JdkHttpClient.simple[IO]
      u <- IO(serverFixture()).map(_.baseUri / "graphql")
      c <- ItcClient.create[IO](u, h)
    } yield c

  def spectroscopy(
    in:       SpectroscopyIntegrationTimeInput,
    expected: Either[String, IntegrationTimeResult]
  ): IO[Unit] =
    itcClient.flatMap {
      _.spectroscopy(in).attempt
        .map(_.leftMap(_.getMessage))
        .assertEquals(expected)
    }

  def imaging(
    in:       ImagingIntegrationTimeInput,
    expected: Either[String, IntegrationTimeResult]
  ): IO[Unit] =
    itcClient.flatMap {
      _.imaging(in).attempt
        .map(_.leftMap(_.getMessage))
        .assertEquals(expected)
    }

  def optimizedSpectroscopyGraph(
    in:       OptimizedSpectroscopyGraphInput,
    expected: Either[String, OptimizedSpectroscopyGraphResult]
  ): IO[Unit] =
    itcClient.flatMap {
      _.optimizedSpectroscopyGraph(in).attempt
        .map(_.leftMap(_.getMessage))
        .assertEquals(expected)
    }

  def versions(
    expected: Either[String, ItcVersions]
  ): IO[Unit] =
    itcClient.flatMap {
      _.versions.attempt
        .map(_.leftMap(_.getMessage))
        .assertEquals(expected)
    }
}
