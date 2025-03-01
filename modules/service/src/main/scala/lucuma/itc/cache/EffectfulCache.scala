// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.cache

import cats.MonadThrow
import cats.syntax.all.*
import natchez.Trace
import org.typelevel.log4cats.Logger

trait EffectfulCache[F[_]: MonadThrow: Trace: Logger, K, V]:
  protected val L: Logger[F] = Logger[F]

  protected def read(key: K): F[Option[V]]

  protected def write(key: K, value: V): F[Unit]

  def flush: F[Unit]

  def getOrInvoke(key: K, effect: F[V]): F[V] =
    val whenFound: F[Unit] =
      L.debug(s"Key [$key] found on cache")

    val whenMissing: F[V] =
      for
        _ <- L.debug(s"Key [$key] not found on cache")
        r <- Trace[F].span("cache-request-call")(effect)
        _ <-
          write(key, r)
            .handleErrorWith(L.error(_)(s"Error writing to cache with key [$key]"))
      yield r

    Trace[F].span("cache-read") {
      for
        _          <- Trace[F].put("cache.key" -> key.toString)
        _          <- L.debug(s"Reading from cache with key [$key]")
        cacheValue <-
          read(key)
            .handleErrorWith: e =>
              L.error(e)(s"Error reading from cache with key [$key]").as(none)
        r          <- cacheValue.fold(whenMissing)(whenFound.as(_))
      yield r
    }
