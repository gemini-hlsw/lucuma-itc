// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.cache

import boopickle.DefaultBasic.*
import cats.Hash
import cats.effect.MonadCancelThrow
import cats.syntax.all.*
import natchez.Trace
import org.typelevel.log4cats.Logger

import java.nio.ByteBuffer
import java.nio.charset.Charset
import scala.concurrent.duration.FiniteDuration

/**
 * Keys are formed with an optional prefix and a hash of the request.
 *
 * Values are stored in binary via boopickle.
 */
trait BinaryEffectfulCache[F[_]: MonadCancelThrow: Trace: Logger]
    extends EffectfulCache[F, Array[Byte], Array[Byte]]:
  protected val KeyCharset = Charset.forName("UTF8")

  private def keyToBinary[K1: Hash](key: K1, keyPrefix: String = ""): Array[Byte] =
    val hash: Int      = Hash[K1].hash(key)
    val keyStr: String = s"$keyPrefix:$hash"
    keyStr.getBytes(KeyCharset)

  private def valueToBinary[V1: Pickler](value: V1): Array[Byte] =
    Pickle.intoBytes(value).compact().array()

  private def valueFromBinary[V1: Pickler](bytes: Array[Byte]): F[V1] =
    Either.catchNonFatal(Unpickle[V1].fromBytes(ByteBuffer.wrap(bytes))).liftTo[F]

  def readBinary[K1: Hash, V1: Pickler](key: K1, keyPrefix: String = ""): F[Option[V1]] =
    read(keyToBinary(key, keyPrefix)).flatMap(_.traverse(valueFromBinary))

  def writeBinary[K1: Hash, V1: Pickler](
    key:       K1,
    value:     V1,
    ttl:       Option[FiniteDuration],
    keyPrefix: String = ""
  ): F[Unit] =
    write(keyToBinary(key, keyPrefix), valueToBinary(value), ttl)

  def getOrInvokeBinary[K1: Hash, V1: Pickler](
    key:       K1,
    effect:    F[V1],
    ttl:       Option[FiniteDuration],
    keyPrefix: String = ""
  ): F[V1] =
    getOrInvoke(keyToBinary(key, keyPrefix), effect.map(valueToBinary), ttl)
      .flatMap(valueFromBinary)
