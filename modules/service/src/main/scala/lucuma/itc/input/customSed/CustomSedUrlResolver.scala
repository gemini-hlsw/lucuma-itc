// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.input.customSed

import cats.effect.Resource
import cats.effect.kernel.Async
import cats.syntax.all.*
import fs2.io.net.Network
import fs2.text
import org.http4s.Request
import org.http4s.Uri
import org.http4s.client.Client
import org.http4s.ember.client.EmberClientBuilder
import org.typelevel.log4cats.Logger

/**
 * Implementation of `CustomSed.Resolver` that uses a URL to fetch the custom SED.
 */
class CustomSedUrlResolver[F[_]: Async: Logger] private (
  client: Client[F]
) extends CustomSedDatResolver[F]:

  protected def datLines(url: CustomSed.Id): F[fs2.Stream[F, String]] =
    for
      _   <- Logger[F].debug(s"Fetching custom SED from $url")
      uri <- Uri.fromString(url).liftTo[F]
    yield client
      .stream(Request(uri = uri))
      .flatMap(_.body)
      .through(text.utf8.decode)
      .through(text.lines)

object CustomSedUrlResolver:
  def apply[F[_]: Async: Network: Logger]: Resource[F, CustomSed.Resolver[F]] =
    EmberClientBuilder
      .default[F]
      .build
      .map(client => new CustomSedUrlResolver(client))
