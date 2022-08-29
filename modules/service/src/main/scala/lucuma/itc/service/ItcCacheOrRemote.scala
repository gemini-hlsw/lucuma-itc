// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import boopickle.DefaultBasic.*
import cats.*
import cats.syntax.all.*
import dev.profunktor.redis4cats.algebra.Flush
import dev.profunktor.redis4cats.algebra.StringCommands
import lucuma.itc.Itc
import lucuma.itc.search.ItcVersions
import lucuma.itc.service.config.ExecutionEnvironment
import lucuma.itc.service.redis.given
import org.typelevel.log4cats.Logger

import java.nio.ByteBuffer
import java.nio.charset.Charset
import scala.concurrent.duration.*

trait ItcCacheOrRemote extends Version:
  val KeyCharset = Charset.forName("UTF8")
  val TTL        = FiniteDuration(10, DAYS)

  def cacheOrRemote[F[_]: Monad, A: Hash, B: Pickler](a: A, request: A => F[B])(
    prefix:                                              String,
    redis:                                               StringCommands[F, Array[Byte], Array[Byte]]
  ): F[B] =
    val hash = Hash[A].hash(a)
    for
      fromRedis <- redis.get(s"$prefix:$hash".getBytes(KeyCharset))
      decoded   <-
        fromRedis
          .flatMap(b => Either.catchNonFatal(Unpickle[B].fromBytes(ByteBuffer.wrap(b))).toOption)
          .pure[F]
      r         <- decoded.map(_.pure[F]).getOrElse(request(a))
      _         <-
        redis
          .setEx(s"$prefix:$hash".getBytes(KeyCharset), Pickle.intoBytes(r).compact().array(), TTL)
          .whenA(fromRedis.isEmpty)
    yield r

  def graphFromCacheOrRemote[F[_]: Monad](
    request: GraphRequest
  )(itc:     Itc[F], redis: StringCommands[F, Array[Byte], Array[Byte]]): F[Itc.GraphResult] =
    val call = (request: GraphRequest) =>
      itc
        .calculateGraph(
          request.targetProfile,
          request.specMode,
          request.constraints,
          request.expTime,
          request.exp
        )

    cacheOrRemote(request, call)("itc:graph:spec", redis)

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
          .setEx("itc:version".getBytes(KeyCharset),
                 version.dataVersion.orEmpty.getBytes(KeyCharset),
                 TTL
          )
          .whenA(fromRedis.isEmpty)
    yield version

  def calcFromCacheOrRemote[F[_]: Monad](
    calcRequest: CalcRequest
  )(itc:         Itc[F], redis: StringCommands[F, Array[Byte], Array[Byte]]): F[Itc.CalcResultWithVersion] =
    val call = (calcRequest: CalcRequest) =>
      itc
        .calculate(
          calcRequest.targetProfile,
          calcRequest.specMode,
          calcRequest.constraints,
          calcRequest.signalToNoise.value
        )

    cacheOrRemote(calcRequest, call)("itc:calc:spec", redis)

  def checkVersionToPurge[F[_]: Monad: Logger](
    redis: StringCommands[F, Array[Byte], Array[Byte]] with Flush[F, Array[Byte]],
    itc:   Itc[F]
  ): F[Unit] =
    val L = Logger[F]
    for
      _              <- L.info("Check for stale cache")
      remoteVersion  <- itc.itcVersions // Remote itc version
      _              <- L.info(s"Remote itc version $remoteVersion")
      fromRedis      <- redis.get("itc:version".getBytes(KeyCharset))
      _              <- L.info(s"Local itc version $remoteVersion")
      versionOnRedis <- fromRedis.map(v => String(v, KeyCharset)).pure[F]
      // if the version changes flush redis
      _              <- (L.info("Flush redis cache on itc version change") *> redis.flushAll)
                          .whenA(versionOnRedis.exists(_ =!= remoteVersion))
      _              <- redis.setEx("itc:version".getBytes(KeyCharset), remoteVersion.getBytes(KeyCharset), TTL)
    yield ()
