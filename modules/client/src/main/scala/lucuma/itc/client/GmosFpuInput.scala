// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client

import cats.syntax.either.*
import cats.syntax.option.*
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosSouthFpu
import lucuma.itc.client.json.syntax.*

object GmosFpuInput {

  final case class North(fpu: Either[GmosCustomMaskInput, GmosNorthFpu]) {
    def customMask: Option[GmosCustomMaskInput] =
      fpu.left.toOption

    def builtin: Option[GmosNorthFpu] =
      fpu.toOption
  }

  object North {

    def customMask(m: GmosCustomMaskInput): North =
      North(m.asLeft)

    def builtin(b: GmosNorthFpu): North =
      North(b.asRight)

    given Encoder[North] with
      def apply(a: North): Json =
        Json.obj(
          a.fpu.fold(
            m => "customMask" -> m.asJson,
            b => "builtin" -> b.asScreamingJson
          )
        )

  }

  final case class South(fpu: Either[GmosCustomMaskInput, GmosSouthFpu]) {
    def customMask: Option[GmosCustomMaskInput] =
      fpu.left.toOption

    def builtin: Option[GmosSouthFpu] =
      fpu.toOption
  }

  object South {

    def customMask(m: GmosCustomMaskInput): South =
      South(m.asLeft)

    def builtin(b: GmosSouthFpu): South =
      South(b.asRight)

    given Encoder[South] with
      def apply(a: South): Json =
        Json.obj(
          a.fpu.fold(
            m => "customMask" -> m.asJson,
            b => "builtin" -> b.asScreamingJson
          )
        )

  }

}
