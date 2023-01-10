// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client

import cats.Eq
import cats.syntax.either.*
import cats.syntax.eq.*
import cats.syntax.functor.*
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Encoder
import io.circe.HCursor
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.GmosSouthGrating
import lucuma.itc.client.json.syntax.*

sealed trait InstrumentMode

object InstrumentMode {

  final case class GmosNorth(
    grating: GmosNorthGrating,
    filter:  Option[GmosNorthFilter],
    fpu:     GmosFpu.North
  ) extends InstrumentMode

  object GmosNorth {

    given Encoder[GmosNorth] with
      def apply(a: GmosNorth): Json =
        Json.fromFields(
          List(
            "grating" -> a.grating.asScreamingJson,
            "fpu"     -> a.fpu.asJson
          ) ++ a.filter.map(_.asScreamingJson).tupleLeft("filter").toList
        )

    given Decoder[GmosNorth] with
      def apply(c: HCursor): Decoder.Result[GmosNorth] =
        for {
          g <- c.downField("grating").as[GmosNorthGrating]
          f <- c.downField("filter").as[Option[GmosNorthFilter]]
          u <- c.downField("fpu").as[GmosFpu.North]
        } yield GmosNorth(g, f, u)

    given Eq[GmosNorth] with
      def eqv(x: GmosNorth, y: GmosNorth): Boolean =
        (x.grating === y.grating) &&
          (x.filter === y.filter) &&
          (x.fpu === y.fpu)

  }

  final case class GmosSouth(
    grating: GmosSouthGrating,
    filter:  Option[GmosSouthFilter],
    fpu:     GmosFpu.South
  ) extends InstrumentMode

  object GmosSouth {

    given Encoder[GmosSouth] with
      def apply(a: GmosSouth): Json =
        Json.fromFields(
          List(
            "grating" -> a.grating.asScreamingJson,
            "fpu"     -> a.fpu.asJson
          ) ++ a.filter.map(_.asScreamingJson).tupleLeft("filter").toList
        )

    given Decoder[GmosSouth] with
      def apply(c: HCursor): Decoder.Result[GmosSouth] =
        for {
          g <- c.downField("grating").as[GmosSouthGrating]
          f <- c.downField("filter").as[Option[GmosSouthFilter]]
          u <- c.downField("fpu").as[GmosFpu.South]
        } yield GmosSouth(g, f, u)

    given Eq[GmosSouth] with
      def eqv(x: GmosSouth, y: GmosSouth): Boolean =
        (x.grating === y.grating) &&
          (x.filter === y.filter) &&
          (x.fpu === y.fpu)

  }

  given Encoder[InstrumentMode] with
    def apply(a: InstrumentMode): Json =
      a match {
        case a @ GmosNorth(_, _, _) => Json.obj("gmosN" -> a.asJson)
        case a @ GmosSouth(_, _, _) => Json.obj("gmosS" -> a.asJson)
      }

  given Decoder[InstrumentMode] with
    def apply(c: HCursor): Decoder.Result[InstrumentMode] =
      for {
        n <- c.downField("gmosN").as[Option[GmosNorth]]
        s <- c.downField("gmosS").as[Option[GmosSouth]]
        m <- (n, s) match {
               case (Some(n), None) => (n: InstrumentMode).asRight
               case (None, Some(s)) => (s: InstrumentMode).asRight
               case _               =>
                 DecodingFailure("Expected exactly one of 'gmosN' or 'gmosS'.", c.history).asLeft
             }
      } yield m

  given Eq[InstrumentMode] with
    def eqv(x: InstrumentMode, y: InstrumentMode): Boolean =
      (x, y) match {
        case (x0: GmosNorth, y0: GmosNorth) => x0 === y0
        case (x0: GmosSouth, y0: GmosSouth) => x0 === y0
        case _                              => false
      }
}
