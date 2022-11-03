// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client

import cats.syntax.functor.*
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.GmosSouthGrating
import lucuma.itc.client.json.syntax.*

sealed trait InstrumentModesInput

object InstrumentModesInput {

  final case class GmosNorthItcInput(
    grating: GmosNorthGrating,
    filter:  Option[GmosNorthFilter],
    fpu:     GmosNorthFpu
  ) extends InstrumentModesInput

  object GmosNorthItcInput {

    given Encoder[GmosNorthItcInput] with
      def apply(a: GmosNorthItcInput): Json =
        Json.obj(
        "gmosN" ->
          Json.fromFields(
            List(
              "grating" -> a.grating.asScreamingJson,
              "fpu"     -> Json.obj("builtin" -> a.fpu.asScreamingJson)
            ) ++ a.filter.map(_.asScreamingJson).tupleLeft("filter").toList
          )
        )

  }

  final case class GmosSouthItcInput(
    grating: GmosSouthGrating,
    filter:  Option[GmosSouthFilter],
    fpu:     GmosSouthFpu
  ) extends InstrumentModesInput

  object GmosSouthItcInput {

    given Encoder[GmosSouthItcInput] with
      def apply(a: GmosSouthItcInput): Json =
        Json.obj(
        "gmosS" ->
          Json.fromFields(
            List(
              "grating" -> a.grating.asScreamingJson,
              "fpu"     -> Json.obj("builtin" -> a.fpu.asScreamingJson)
            ) ++ a.filter.map(_.asScreamingJson).tupleLeft("filter").toList
          )
        )

  }

  given Encoder[InstrumentModesInput] with
    def apply(a: InstrumentModesInput): Json =
      a match {
        case a @ GmosNorthItcInput(_, _, _) => a.asJson
        case a @ GmosSouthItcInput(_, _, _) => a.asJson
      }

}