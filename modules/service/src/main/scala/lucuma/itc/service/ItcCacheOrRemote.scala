// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import boopickle.DefaultBasic.*
import cats._
import cats.syntax.all._
import dev.profunktor.redis4cats.algebra.StringCommands
import lucuma.itc.Itc
import lucuma.itc.search.ItcVersions
import lucuma.itc.service.config.ExecutionEnvironment
import lucuma.itc.service.redis.given

import java.nio.ByteBuffer
import java.nio.charset.Charset

trait ItcCacheOrRemote extends Version:
  val KeyCharset = Charset.forName("UTF8")

  def graphFromCacheOrRemote[F[_]: Monad](
    request: GraphRequest
  )(itc:     Itc[F], redis: StringCommands[F, Array[Byte], Array[Byte]]): F[Itc.GraphResult] =
    val hash = Hash[GraphRequest].hash(request)
    for
      fromRedis <- redis.get(s"itc:graph:$hash".getBytes(KeyCharset))
      decoded   <-
        fromRedis
          .flatMap(b =>
            Either.catchNonFatal(Unpickle[Itc.GraphResult].fromBytes(ByteBuffer.wrap(b))).toOption
          )
          .pure[F]
      r         <-
        decoded
          .map(_.pure[F])
          .getOrElse(
            // Call old itc
            itc
              .calculateGraph(
                request.targetProfile,
                request.specMode,
                request.constraints,
                request.expTime,
                request.exp
              )
          )
      _         <-
        redis
          .set(s"itc:graph:$hash".getBytes(KeyCharset), Pickle.intoBytes(r).compact().array())
          .whenA(fromRedis.isEmpty)
    yield r

  def versionFromCacheOrRemote[F[_]: Monad](
    environment: ExecutionEnvironment,
    redis:       StringCommands[F, Array[Byte], Array[Byte]],
    itc:         Itc[F]
  ): F[ItcVersions] =
    for
      fromRedis <- redis.get("itc:version".getBytes(KeyCharset))
      version   <- fromRedis.fold(
                     itc.itcVersions
                       .map { r =>
                         ItcVersions(version(environment).value, r.some)
                       }
                   )(v => ItcVersions(version(environment).value, String(v, KeyCharset).some).pure[F])
      _         <-
        redis
          .set("itc:version".getBytes(KeyCharset), version.dataVersion.orEmpty.getBytes(KeyCharset))
          .whenA(fromRedis.isEmpty)
    yield version
