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
import clue.TransactionalClient
import clue.http4s.Http4sBackend
import io.circe.syntax.*
import lucuma.core.model.Observation
import org.http4s.Uri
import org.http4s.client.Client
import org.typelevel.log4cats.Logger

/**
 * Client for calling the ITC on the JVM.
 */
trait ItcClient[F[_]] {

  // TODO: chart

  def spectroscopy(
    input:    SpectroscopyModeInput,
    useCache: Boolean = true
  ): F[SpectroscopyResult]

  def versions: F[ItcVersions]

}

object ItcClient {

  def create[F[_]: Async: Logger](
    uri:    Uri,
    client: Client[F]
  ): F[ItcClient[F]] =
    for {
      cache <- ItcCache.simple[F, SpectroscopyModeInput, SpectroscopyResult]
      http  <- TransactionalClient.of[F, Unit](uri)(Async[F], Http4sBackend(client), Logger[F])
    } yield new ItcClient[F] {
      override def spectroscopy(
        input:    SpectroscopyModeInput,
        useCache: Boolean = true
      ): F[SpectroscopyResult] = {

        val callOut: F[SpectroscopyResult] =
          http.request(SpectroscopyQuery)(input)

        for {
          _ <- Logger[F].info(s"ITC Input: \n${input.asJson.spaces2}")
          v <- if (useCache) cache.getOrCalcF(input)(callOut)
               else callOut.flatTap(cache.put(input))
          _ <- Logger[F].info(s"ITC Result:\n$v")
        } yield v
      }

      override val versions: F[ItcVersions] =
        http.request(VersionsQuery)

    }

}
