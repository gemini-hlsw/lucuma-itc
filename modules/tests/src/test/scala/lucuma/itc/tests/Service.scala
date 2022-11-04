// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.tests

import cats.effect.IO
import cats.syntax.option.*
import lucuma.graphql.routes.GrackleGraphQLService
import lucuma.graphql.routes.Routes
import lucuma.itc.Itc
import lucuma.itc.service.ItcMapping
import lucuma.itc.service.config.ExecutionEnvironment
import natchez.Trace
import org.http4s.HttpRoutes
import org.http4s.server.websocket.WebSocketBuilder2
import org.typelevel.log4cats.Logger

def service(itc: Itc[IO])(using Logger[IO], Trace[IO]): IO[HttpRoutes[IO]] =
  for {
    wsb <- WebSocketBuilder2[IO]
    map <- ItcMapping[IO](
             ExecutionEnvironment.Local,
             new NoOpRedis[IO, Array[Byte], Array[Byte]](),
             itc
           )
  } yield Routes.forService(_ => IO.pure(GrackleGraphQLService(map).some), wsb)