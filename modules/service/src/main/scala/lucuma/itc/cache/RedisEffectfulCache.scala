// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.cache

import cats.effect.Async
import cats.effect.MonadCancelThrow
import cats.syntax.all.*
import dev.profunktor.redis4cats.algebra.Flush
import dev.profunktor.redis4cats.algebra.StringCommands
import io.chrisdavenport.keysemaphore.KeySemaphore
import natchez.Trace
import org.typelevel.log4cats.Logger

import scala.concurrent.duration.*

trait RedisEffectfulCache[F[_]: MonadCancelThrow: Trace: Logger](
  redis:                      StringCommands[F, Array[Byte], Array[Byte]] & Flush[F, Array[Byte]],
  protected val keySemaphore: KeySemaphore[F, Array[Byte]]
) extends BinaryEffectfulCache[F]:
  // Time to live for entries. The idea of having such a long TTL is that eviction is based on LRU
  // and cache size restriction. As such, Redis should be configured to use an LRU eviction policy.
  private val TTL = FiniteDuration(365, DAYS)

  override protected def read(key: Array[Byte]): F[Option[Array[Byte]]] =
    redis.get(key)

  override protected def write(key: Array[Byte], value: Array[Byte]): F[Unit] =
    redis.setEx(key, value, TTL)

  override def flush: F[Unit] = redis.flushAll

object RedisEffectfulCache:
  def apply[F[_]: Async: Trace: Logger](
    redis: StringCommands[F, Array[Byte], Array[Byte]] & Flush[F, Array[Byte]]
  ): F[RedisEffectfulCache[F]] =
    KeySemaphore
      .of[F, Array[Byte]](_ => 1L) // 1 permit per key
      .map: keySemaphore =>
        new RedisEffectfulCache(redis, keySemaphore) {}
