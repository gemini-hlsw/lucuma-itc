// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client

import cats.effect.*
import cats.implicits.*
import com.comcast.ip4s.*
import lucuma.itc.Itc
import lucuma.itc.ItcVersions
import lucuma.itc.tests.EmissionLineMockItc
import lucuma.itc.tests.MockItc
import munit.CatsEffectSuite
import munit.catseffect.IOFixture
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

  private def httpApp(backend: Itc[IO]): Resource[IO, WebSocketBuilder2[IO] => HttpApp[IO]] =
    Resource.eval(lucuma.itc.tests.app(backend))

  private def server(backend: Itc[IO], port: Int): Resource[IO, Server] =
    httpApp(backend).flatMap: app =>
      EmberServerBuilder
        .default[IO]
        .withHost(ipv4"0.0.0.0")
        .withPort(Port.fromInt(port).get)
        .withHttpWebSocketApp(app)
        .withShutdownTimeout(2.seconds)
        .build

  private def serverFixture(backend: Itc[IO], port: Int): IOFixture[Server] =
    ResourceSuiteLocalFixture("server", server(backend, port))

  private val bandNormalizedFixture = serverFixture(MockItc, 8080)
  private val emissionLineFixture   = serverFixture(EmissionLineMockItc, 8081)

  override def munitFixtures = List(bandNormalizedFixture, emissionLineFixture)

  private def itcClient(fixture: IOFixture[Server]): IO[ItcClient[IO]] =
    for
      h <- JdkHttpClient.simple[IO]
      u <- IO(fixture()).map(_.baseUri / "graphql")
      c <- ItcClient.create[IO](u, h)
    yield c

  private val bandNormalizedClient = itcClient(bandNormalizedFixture)
  private val emissionLineClient   = itcClient(emissionLineFixture)

  def spectroscopy(
    in:       SpectroscopyIntegrationTimeInput,
    expected: Either[String, IntegrationTimeResult]
  ): IO[Unit] =
    bandNormalizedClient.flatMap:
      _.spectroscopy(in).attempt
        .map(_.leftMap(_.getMessage))
        .assertEquals(expected)

  def imaging(
    in:       ImagingIntegrationTimeInput,
    expected: Either[String, IntegrationTimeResult]
  ): IO[Unit] =
    bandNormalizedClient.flatMap:
      _.imaging(in).attempt
        .map(_.leftMap(_.getMessage))
        .assertEquals(expected)

  def spectroscopyGraphs(
    in:       SpectroscopyGraphsInput,
    expected: Either[String, SpectroscopyGraphsResult]
  ): IO[Unit] =
    bandNormalizedClient.flatMap:
      _.spectroscopyGraphs(in).attempt
        .map(_.leftMap(_.getMessage))
        .assertEquals(expected)

  def versions(
    expected: Either[String, ItcVersions]
  ): IO[Unit] =
    bandNormalizedClient.flatMap:
      _.versions.attempt
        .map(_.leftMap(_.getMessage))
        .assertEquals(expected)

  def spectroscopyEmissionLines(
    in:       SpectroscopyIntegrationTimeInput,
    expected: Either[String, IntegrationTimeResult]
  ): IO[Unit] =
    emissionLineClient.flatMap:
      _.spectroscopy(in).attempt
        .map(_.leftMap(_.getMessage))
        .assertEquals(expected)
}
