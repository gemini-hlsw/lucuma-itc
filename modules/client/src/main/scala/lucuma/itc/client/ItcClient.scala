// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client

import cats.Applicative
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
import org.http4s.jdkhttpclient.JdkHttpClient
import org.typelevel.log4cats.Logger

/**
 * Client for calling the ITC on the JVM.
 */
trait ItcClient[F[_]] {

  // TODO: chart

  def spectroscopy(
    input:    SpectroscopyModeInput,
    useCache: Boolean = true
  ): F[Either[Throwable, SpectroscopyResult]]

  def versions: F[Either[Throwable, ItcVersions]]

}

object ItcClient {

  def create[F[_]: Async: Logger](
    uri:    Uri,
    client: Client[F]
  ): F[ItcClient[F]] =
    Ref
      .of[F, Map[SpectroscopyModeInput, Either[Throwable, SpectroscopyResult]]](Map.empty)
      .map { cache =>
        // TODO: Cache contains failed results and is not flushed until the next server restart.
        // TOOD: Likely we don't want to cache failures and should flush at least when the version changes.

        new ItcClient[F] {
          val httpClient: Resource[F, TransactionalClient[F, Unit]] =
            Resource.eval(
              TransactionalClient.of[F, Unit](uri)(Async[F], Http4sBackend(client), Logger[F])
            )

          override def spectroscopy(
            input:    SpectroscopyModeInput,
            useCache: Boolean = true
          ): F[Either[Throwable, SpectroscopyResult]] = {

            val callAndCache: F[Either[Throwable, SpectroscopyResult]] =
              for {
                r <- httpClient.use(_.request(SpectroscopyQuery)(input)).attempt
                rʹ = r.flatMap(
                       _.headOption.toRight(new RuntimeException("No results returned by ITC."))
                     )
                _ <- cache.update(_ + (input -> rʹ))
              } yield rʹ

            for {
              _    <- Logger[F].info(s"ITC Input: \n${input.asJson.spaces2}")
              cval <- if (useCache) cache.get.map(_.get(input)) else Applicative[F].pure(None)
              res  <- cval.fold(callAndCache)(Applicative[F].pure)
              _    <- Logger[F].info(s"ITC Result (${cval.fold("remote")(_ => "cached")}):\n$res")
            } yield res
          }

          override val versions: F[Either[Throwable, ItcVersions]] =
            httpClient.use(_.request(VersionsQuery)).attempt

        }
      }

}
