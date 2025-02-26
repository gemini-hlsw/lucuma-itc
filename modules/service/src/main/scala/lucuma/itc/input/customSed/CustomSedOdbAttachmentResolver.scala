// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.input.customSed

import cats.effect.Resource
import cats.effect.kernel.Async
import cats.syntax.all.*
import fs2.io.net.Network
import fs2.text
import org.http4s.AuthScheme
import org.http4s.Credentials
import org.http4s.Headers
import org.http4s.Request
import org.http4s.Uri
import org.http4s.client.Client
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.headers.Authorization
import org.typelevel.log4cats.Logger

/**
 * Implementation of `CustomSed.Resolver` that uses a URL to fetch the custom SED.
 */
class CustomSedOdbAttachmentResolver[F[_]: Async: Logger] private (
  client:     Client[F],
  odbBaseUrl: Uri,
  authToken:  String
) extends CustomSedDatResolver[F]:

  private def getRequest(id: CustomSed.Id): Request[F] =
    Request(
      uri = odbBaseUrl / "attachments" / id.show,
      headers = Headers:
        Authorization:
          Credentials.Token(AuthScheme.Bearer, authToken)
    )

  protected def datLines(id: CustomSed.Id): F[fs2.Stream[F, String]] =
    for _ <- Logger[F].debug(s"Fetching custom SED for id [$id]")
    yield client
      .stream(getRequest(id))
      .flatMap(_.body)
      .through(text.utf8.decode)
      .through(text.lines)

object CustomSedOdbAttachmentResolver:
  def apply[F[_]: Async: Network: Logger](
    odbBaseUrl: Uri,
    authToken:  String
  ): Resource[F, CustomSed.Resolver[F]] =
    EmberClientBuilder
      .default[F]
      .build
      .map(client => new CustomSedOdbAttachmentResolver(client, odbBaseUrl, authToken))
