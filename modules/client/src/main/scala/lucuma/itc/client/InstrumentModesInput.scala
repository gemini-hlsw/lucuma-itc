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

  final case class GmosNorth(
    grating: GmosNorthGrating,
    filter:  Option[GmosNorthFilter],
    fpu:     GmosFpuInput.North
  ) extends InstrumentModesInput

  object GmosNorth {

    given Encoder[GmosNorth] with
      def apply(a: GmosNorth): Json =
        Json.obj(
          "gmosN" ->
            Json.fromFields(
              List(
                "grating" -> a.grating.asScreamingJson,
                "fpu"     -> a.fpu.asJson
              ) ++ a.filter.map(_.asScreamingJson).tupleLeft("filter").toList
            )
        )

  }

  final case class GmosSouth(
    grating: GmosSouthGrating,
    filter:  Option[GmosSouthFilter],
    fpu:     GmosFpuInput.South
  ) extends InstrumentModesInput

  object GmosSouth {

    given Encoder[GmosSouth] with
      def apply(a: GmosSouth): Json =
        Json.obj(
          "gmosS" ->
            Json.fromFields(
              List(
                "grating" -> a.grating.asScreamingJson,
                "fpu"     -> a.fpu.asJson
              ) ++ a.filter.map(_.asScreamingJson).tupleLeft("filter").toList
            )
        )

  }

  given Encoder[InstrumentModesInput] with
    def apply(a: InstrumentModesInput): Json =
      a match {
        case a @ GmosNorth(_, _, _) => a.asJson
        case a @ GmosSouth(_, _, _) => a.asJson
      }

}
