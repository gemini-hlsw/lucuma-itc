// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client

import cats.Applicative
import cats.ApplicativeError
import cats.effect.Async
import cats.effect.Ref
import cats.effect.Resource
import cats.syntax.applicativeError.*
import cats.syntax.apply.*
import cats.syntax.either.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.option.*
import clue.http4s.Http4sHttpBackend
import clue.http4s.Http4sHttpClient
import io.circe.syntax.*
import lucuma.core.model.Observation
import lucuma.itc.client.OptimizedSpectroscopyGraphResult
import org.http4s.Uri
import org.http4s.client.Client
import org.typelevel.log4cats.Logger

/**
 * Client for calling the ITC on the JVM.
 */
trait ItcClient[F[_]] {

  def integrationTime(
    input:    IntegrationTimeInput,
    useCache: Boolean = true
  ): F[IntegrationTimeResult]

  def optimizedSpectroscopyGraph(
    input:    OptimizedSpectroscopyGraphInput,
    useCache: Boolean = true
  ): F[OptimizedSpectroscopyGraphResult]

  def versions: F[ItcVersions]

}

object ItcClient {
  def apply[F[_]](using ev: ItcClient[F]): ItcClient[F] = ev

  def create[F[_]: Async: Logger](
    uri:    Uri,
    client: Client[F]
  ): F[ItcClient[F]] =
    for {
      cache      <- ItcCache.simple[F, IntegrationTimeInput, IntegrationTimeResult]
      graphCache <-
        ItcCache.simple[F, OptimizedSpectroscopyGraphInput, OptimizedSpectroscopyGraphResult]
      http       <- Http4sHttpClient.of[F, Unit](uri)(Async[F], Http4sHttpBackend(client), Logger[F])
    } yield new ItcClient[F] {
      override def integrationTime(
        input:    IntegrationTimeInput,
        useCache: Boolean = true
      ): F[IntegrationTimeResult] = {

        val callOut: F[IntegrationTimeResult] =
          http.request(ITCQueries).withInput(input)

        for {
          _ <- Logger[F].debug(s"ITC Input: \n${input.asJson.spaces2}")
          v <- if (useCache) cache.getOrCalcF(input)(callOut)
               else callOut.flatTap(cache.put(input))
          _ <- Logger[F].debug(s"ITC Result:\n$v")
        } yield v
      }

      override val versions: F[ItcVersions] =
        http.request(VersionsQuery)

      def optimizedSpectroscopyGraph(
        input:    OptimizedSpectroscopyGraphInput,
        useCache: Boolean = true
      ): F[OptimizedSpectroscopyGraphResult] = {
        val callOut: F[OptimizedSpectroscopyGraphResult] =
          http.request(SpectroscopyGraphQuery).withInput(input)

        for {
          _ <- Logger[F].debug(s"ITC Input: \n${input.asJson.spaces2}")
          v <- if (useCache) graphCache.getOrCalcF(input)(callOut)
               else callOut.flatTap(graphCache.put(input))
          _ <- Logger[F].debug(s"ITC Result:\n$v")
        } yield v
      }

    }

}
