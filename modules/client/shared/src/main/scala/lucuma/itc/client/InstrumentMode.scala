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
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.math.Wavelength
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.itc.client.json.syntax.*
import lucuma.odb.json.gmos.given
import lucuma.odb.json.wavelength.transport.given
import monocle.Prism
import monocle.macros.GenPrism

sealed trait InstrumentMode

object InstrumentMode {

  final case class GmosNorthSpectroscopy(
    centralWavelength: Wavelength,
    grating:           GmosNorthGrating,
    filter:            Option[GmosNorthFilter],
    fpu:               GmosFpu.North,
    ccdMode:           Option[GmosCcdMode],
    roi:               Option[GmosRoi]
  ) extends InstrumentMode

  object GmosNorthSpectroscopy {

    given Encoder[GmosNorthSpectroscopy] with
      def apply(a: GmosNorthSpectroscopy): Json =
        Json.fromFields(
          List(
            "centralWavelength" -> a.centralWavelength.asJson,
            "grating"           -> a.grating.asScreamingJson,
            "fpu"               -> a.fpu.asJson,
            "ccdMode"           -> a.ccdMode.asJson,
            "roi"               -> a.roi.asJson
          ) ++ a.filter.map(_.asScreamingJson).tupleLeft("filter").toList
        )

    given Decoder[GmosNorthSpectroscopy] with
      def apply(c: HCursor): Decoder.Result[GmosNorthSpectroscopy] =
        for
          cw <- c.downField("centralWavelength").as[Wavelength]
          g  <- c.downField("grating").as[GmosNorthGrating]
          f  <- c.downField("filter").as[Option[GmosNorthFilter]]
          u  <- c.downField("fpu").as[GmosFpu.North]
          d  <- c.downField("ccdMode").as[Option[GmosCcdMode]]
          r  <- c.downField("roi").as[Option[GmosRoi]]
        yield GmosNorthSpectroscopy(cw, g, f, u, d, r)

    given Eq[GmosNorthSpectroscopy] with
      def eqv(x: GmosNorthSpectroscopy, y: GmosNorthSpectroscopy): Boolean =
        (x.grating === y.grating) &&
          (x.filter === y.filter) &&
          (x.fpu === y.fpu)

  }

  final case class GmosSouthSpectroscopy(
    centralWavelength: Wavelength,
    grating:           GmosSouthGrating,
    filter:            Option[GmosSouthFilter],
    fpu:               GmosFpu.South,
    ccdMode:           Option[GmosCcdMode],
    roi:               Option[GmosRoi]
  ) extends InstrumentMode

  object GmosSouthSpectroscopy {

    given Encoder[GmosSouthSpectroscopy] with
      def apply(a: GmosSouthSpectroscopy): Json =
        Json.fromFields(
          List(
            "centralWavelength" -> a.centralWavelength.asJson,
            "grating"           -> a.grating.asScreamingJson,
            "fpu"               -> a.fpu.asJson,
            "ccdMode"           -> a.ccdMode.asJson,
            "roi"               -> a.roi.asJson
          ) ++ a.filter.map(_.asScreamingJson).tupleLeft("filter").toList
        )

    given Decoder[GmosSouthSpectroscopy] with
      def apply(c: HCursor): Decoder.Result[GmosSouthSpectroscopy] =
        for
          cw <- c.downField("centralWavelength").as[Wavelength]
          g  <- c.downField("grating").as[GmosSouthGrating]
          f  <- c.downField("filter").as[Option[GmosSouthFilter]]
          u  <- c.downField("fpu").as[GmosFpu.South]
          d  <- c.downField("ccdMode").as[Option[GmosCcdMode]]
          r  <- c.downField("roi").as[Option[GmosRoi]]
        yield GmosSouthSpectroscopy(cw, g, f, u, d, r)

    given Eq[GmosSouthSpectroscopy] with
      def eqv(x: GmosSouthSpectroscopy, y: GmosSouthSpectroscopy): Boolean =
        (x.grating === y.grating) &&
          (x.filter === y.filter) &&
          (x.fpu === y.fpu)

  }

  given Encoder[InstrumentMode] with
    def apply(a: InstrumentMode): Json =
      a match {
        case a @ GmosNorthSpectroscopy(_, _, _, _, _, _) =>
          Json.obj("gmosNSpectroscopy" -> a.asJson)
        case a @ GmosSouthSpectroscopy(_, _, _, _, _, _) =>
          Json.obj("gmosSSpectroscopy" -> a.asJson)
        case a @ GmosNorthImaging(_, _)                  => Json.obj("gmosNImaging" -> a.asJson)
        case a @ GmosSouthImaging(_, _)                  => Json.obj("gmosSImaging" -> a.asJson)
      }

  given Decoder[InstrumentMode] with
    def apply(c: HCursor): Decoder.Result[InstrumentMode] =
      for
        ns <- c.downField("gmosNSpectroscopy").as[Option[GmosNorthSpectroscopy]]
        ss <- c.downField("gmosSSpectroscopy").as[Option[GmosSouthSpectroscopy]]
        ni <- c.downField("gmosNImaging").as[Option[GmosNorthImaging]]
        si <- c.downField("gmosSImaging").as[Option[GmosSouthImaging]]
        m  <- (ns, ss, ni, si) match
                case (Some(n), None, None, None) => (n: InstrumentMode).asRight
                case (None, Some(s), None, None) => (s: InstrumentMode).asRight
                case (None, None, Some(s), None) => (s: InstrumentMode).asRight
                case (None, None, None, Some(s)) => (s: InstrumentMode).asRight
                case _                           =>
                  DecodingFailure("Expected exactly one of 'gmosN' or 'gmosS'.", c.history).asLeft
      yield m

  given Eq[InstrumentMode] with
    def eqv(x: InstrumentMode, y: InstrumentMode): Boolean =
      (x, y) match
        case (x0: GmosNorthSpectroscopy, y0: GmosNorthSpectroscopy) => x0 === y0
        case (x0: GmosSouthSpectroscopy, y0: GmosSouthSpectroscopy) => x0 === y0
        case (x0: GmosNorthImaging, y0: GmosNorthImaging)           => x0 === y0
        case (x0: GmosSouthImaging, y0: GmosSouthImaging)           => x0 === y0
        case _                                                      => false

  final case class GmosNorthImaging(
    filter:  GmosNorthFilter,
    ccdMode: Option[GmosCcdMode]
  ) extends InstrumentMode

  object GmosNorthImaging {

    given Encoder[GmosNorthImaging] with
      def apply(a: GmosNorthImaging): Json =
        Json.obj(
          "filter"  -> a.filter.asScreamingJson,
          "ccdMode" -> a.ccdMode.asJson
        )

    given Decoder[GmosNorthImaging] with
      def apply(c: HCursor): Decoder.Result[GmosNorthImaging] =
        for
          f <- c.downField("filter").as[GmosNorthFilter]
          c <- c.downField("ccdMode").as[Option[GmosCcdMode]]
        yield GmosNorthImaging(f, c)

    given Eq[GmosNorthImaging] with
      def eqv(x: GmosNorthImaging, y: GmosNorthImaging): Boolean =
        x.filter === y.filter

  }

  final case class GmosSouthImaging(
    filter:  GmosSouthFilter,
    ccdMode: Option[GmosCcdMode]
  ) extends InstrumentMode

  object GmosSouthImaging {

    given Encoder[GmosSouthImaging] with
      def apply(a: GmosSouthImaging): Json =
        Json.obj(
          "filter"  -> a.filter.asScreamingJson,
          "ccdMode" -> a.ccdMode.asJson
        )

    given Decoder[GmosSouthImaging] with
      def apply(c: HCursor): Decoder.Result[GmosSouthImaging] =
        for
          f <- c.downField("filter").as[GmosSouthFilter]
          c <- c.downField("ccdMode").as[Option[GmosCcdMode]]
        yield GmosSouthImaging(f, c)

    given Eq[GmosSouthImaging] with
      def eqv(x: GmosSouthImaging, y: GmosSouthImaging): Boolean =
        x.filter === y.filter

  }

  val gmosNorthSpectroscopy: Prism[InstrumentMode, GmosNorthSpectroscopy] =
    GenPrism[InstrumentMode, GmosNorthSpectroscopy]

  val gmosSouthSpectroscopy: Prism[InstrumentMode, GmosSouthSpectroscopy] =
    GenPrism[InstrumentMode, GmosSouthSpectroscopy]

  val gmosNorthImaging: Prism[InstrumentMode, GmosNorthImaging] =
    GenPrism[InstrumentMode, GmosNorthImaging]

  val gmosSouthImaging: Prism[InstrumentMode, GmosSouthImaging] =
    GenPrism[InstrumentMode, GmosSouthImaging]
}
