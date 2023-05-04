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
import monocle.Prism
import monocle.macros.GenPrism

sealed trait InstrumentMode

object InstrumentMode {

  final case class GmosNorthSpectroscopy(
    grating: GmosNorthGrating,
    filter:  Option[GmosNorthFilter],
    fpu:     GmosFpu.North
  ) extends InstrumentMode

  object GmosNorthSpectroscopy {

    given Encoder[GmosNorthSpectroscopy] with
      def apply(a: GmosNorthSpectroscopy): Json =
        Json.fromFields(
          List(
            "grating" -> a.grating.asScreamingJson,
            "fpu"     -> a.fpu.asJson
          ) ++ a.filter.map(_.asScreamingJson).tupleLeft("filter").toList
        )

    given Decoder[GmosNorthSpectroscopy] with
      def apply(c: HCursor): Decoder.Result[GmosNorthSpectroscopy] =
        for {
          g <- c.downField("grating").as[GmosNorthGrating]
          f <- c.downField("filter").as[Option[GmosNorthFilter]]
          u <- c.downField("fpu").as[GmosFpu.North]
        } yield GmosNorthSpectroscopy(g, f, u)

    given Eq[GmosNorthSpectroscopy] with
      def eqv(x: GmosNorthSpectroscopy, y: GmosNorthSpectroscopy): Boolean =
        (x.grating === y.grating) &&
          (x.filter === y.filter) &&
          (x.fpu === y.fpu)

  }

  final case class GmosSouthSpectroscopy(
    grating: GmosSouthGrating,
    filter:  Option[GmosSouthFilter],
    fpu:     GmosFpu.South
  ) extends InstrumentMode

  object GmosSouthSpectroscopy {

    given Encoder[GmosSouthSpectroscopy] with
      def apply(a: GmosSouthSpectroscopy): Json =
        Json.fromFields(
          List(
            "grating" -> a.grating.asScreamingJson,
            "fpu"     -> a.fpu.asJson
          ) ++ a.filter.map(_.asScreamingJson).tupleLeft("filter").toList
        )

    given Decoder[GmosSouthSpectroscopy] with
      def apply(c: HCursor): Decoder.Result[GmosSouthSpectroscopy] =
        for {
          g <- c.downField("grating").as[GmosSouthGrating]
          f <- c.downField("filter").as[Option[GmosSouthFilter]]
          u <- c.downField("fpu").as[GmosFpu.South]
        } yield GmosSouthSpectroscopy(g, f, u)

    given Eq[GmosSouthSpectroscopy] with
      def eqv(x: GmosSouthSpectroscopy, y: GmosSouthSpectroscopy): Boolean =
        (x.grating === y.grating) &&
          (x.filter === y.filter) &&
          (x.fpu === y.fpu)

  }

  given Encoder[InstrumentMode] with
    def apply(a: InstrumentMode): Json =
      a match {
        case a @ GmosNorthSpectroscopy(_, _, _) => Json.obj("gmosNSpectroscopy" -> a.asJson)
        case a @ GmosSouthSpectroscopy(_, _, _) => Json.obj("gmosSSpectroscopy" -> a.asJson)
      }

  given Decoder[InstrumentMode] with
    def apply(c: HCursor): Decoder.Result[InstrumentMode] =
      for {
        n <- c.downField("gmosN").as[Option[GmosNorthSpectroscopy]]
        s <- c.downField("gmosS").as[Option[GmosSouthSpectroscopy]]
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
        case (x0: GmosNorthSpectroscopy, y0: GmosNorthSpectroscopy) => x0 === y0
        case (x0: GmosSouthSpectroscopy, y0: GmosSouthSpectroscopy) => x0 === y0
        case _                                                      => false
      }

  val gmosNorth: Prism[InstrumentMode, GmosNorthSpectroscopy] =
    GenPrism[InstrumentMode, GmosNorthSpectroscopy]

  val gmosSouth: Prism[InstrumentMode, GmosSouthSpectroscopy] =
    GenPrism[InstrumentMode, GmosSouthSpectroscopy]

}
