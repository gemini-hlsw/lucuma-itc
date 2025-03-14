// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client

import cats.Eq
import cats.derived.*
import cats.syntax.either.*
import cats.syntax.eq.*
import cats.syntax.functor.*
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.F2Disperser
import lucuma.core.enums.F2Filter
import lucuma.core.enums.F2Fpu
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

  case class GmosNorthSpectroscopy(
    centralWavelength: Wavelength,
    grating:           GmosNorthGrating,
    filter:            Option[GmosNorthFilter],
    fpu:               GmosFpu.North,
    ccdMode:           Option[GmosCcdMode],
    roi:               Option[GmosRoi]
  ) extends InstrumentMode derives Eq

  object GmosNorthSpectroscopy {

    given Encoder[GmosNorthSpectroscopy] = a =>
      Json.fromFields(
        List(
          "centralWavelength" -> a.centralWavelength.asJson,
          "grating"           -> a.grating.asScreamingJson,
          "fpu"               -> a.fpu.asJson,
          "ccdMode"           -> a.ccdMode.asJson,
          "roi"               -> a.roi.asJson
        ) ++ a.filter.map(_.asScreamingJson).tupleLeft("filter").toList
      )

    given Decoder[GmosNorthSpectroscopy] = c =>
      for
        cw <- c.downField("centralWavelength").as[Wavelength]
        g  <- c.downField("grating").as[GmosNorthGrating]
        f  <- c.downField("filter").as[Option[GmosNorthFilter]]
        u  <- c.downField("fpu").as[GmosFpu.North]
        d  <- c.downField("ccdMode").as[Option[GmosCcdMode]]
        r  <- c.downField("roi").as[Option[GmosRoi]]
      yield GmosNorthSpectroscopy(cw, g, f, u, d, r)

  }

  case class GmosSouthSpectroscopy(
    centralWavelength: Wavelength,
    grating:           GmosSouthGrating,
    filter:            Option[GmosSouthFilter],
    fpu:               GmosFpu.South,
    ccdMode:           Option[GmosCcdMode],
    roi:               Option[GmosRoi]
  ) extends InstrumentMode derives Eq

  object GmosSouthSpectroscopy:

    given Encoder[GmosSouthSpectroscopy] = a =>
      Json.fromFields(
        List(
          "centralWavelength" -> a.centralWavelength.asJson,
          "grating"           -> a.grating.asScreamingJson,
          "fpu"               -> a.fpu.asJson,
          "ccdMode"           -> a.ccdMode.asJson,
          "roi"               -> a.roi.asJson
        ) ++ a.filter.map(_.asScreamingJson).tupleLeft("filter").toList
      )

    given Decoder[GmosSouthSpectroscopy] = c =>
      for
        cw <- c.downField("centralWavelength").as[Wavelength]
        g  <- c.downField("grating").as[GmosSouthGrating]
        f  <- c.downField("filter").as[Option[GmosSouthFilter]]
        u  <- c.downField("fpu").as[GmosFpu.South]
        d  <- c.downField("ccdMode").as[Option[GmosCcdMode]]
        r  <- c.downField("roi").as[Option[GmosRoi]]
      yield GmosSouthSpectroscopy(cw, g, f, u, d, r)

  case class Flamingos2Spectroscopy(
    disperser: F2Disperser,
    filter:    F2Filter,
    fpu:       F2Fpu
  ) extends InstrumentMode derives Eq

  object Flamingos2Spectroscopy:

    given Encoder[Flamingos2Spectroscopy] = a =>
      Json.fromFields(
        List(
          "disperser" -> a.disperser.asScreamingJson,
          "fpu"       -> a.fpu.asJson,
          "filter"    -> a.filter.asJson
        )
      )

    given Decoder[Flamingos2Spectroscopy] = c =>
      for
        g <- c.downField("disperser").as[F2Disperser]
        f <- c.downField("filter").as[F2Filter]
        u <- c.downField("fpu").as[F2Fpu]
      yield Flamingos2Spectroscopy(g, f, u)

  case class GmosNorthImaging(
    filter:  GmosNorthFilter,
    ccdMode: Option[GmosCcdMode]
  ) extends InstrumentMode derives Eq

  object GmosNorthImaging:

    given Encoder[GmosNorthImaging] = a =>
      Json.obj(
        "filter"  -> a.filter.asScreamingJson,
        "ccdMode" -> a.ccdMode.asJson
      )

    given Decoder[GmosNorthImaging] = c =>
      for
        f <- c.downField("filter").as[GmosNorthFilter]
        c <- c.downField("ccdMode").as[Option[GmosCcdMode]]
      yield GmosNorthImaging(f, c)

  case class GmosSouthImaging(
    filter:  GmosSouthFilter,
    ccdMode: Option[GmosCcdMode]
  ) extends InstrumentMode derives Eq

  object GmosSouthImaging:

    given Encoder[GmosSouthImaging] = a =>
      Json.obj(
        "filter"  -> a.filter.asScreamingJson,
        "ccdMode" -> a.ccdMode.asJson
      )

    given Decoder[GmosSouthImaging] = c =>
      for
        f <- c.downField("filter").as[GmosSouthFilter]
        c <- c.downField("ccdMode").as[Option[GmosCcdMode]]
      yield GmosSouthImaging(f, c)

  val gmosNorthSpectroscopy: Prism[InstrumentMode, GmosNorthSpectroscopy] =
    GenPrism[InstrumentMode, GmosNorthSpectroscopy]

  val gmosSouthSpectroscopy: Prism[InstrumentMode, GmosSouthSpectroscopy] =
    GenPrism[InstrumentMode, GmosSouthSpectroscopy]

  val gmosNorthImaging: Prism[InstrumentMode, GmosNorthImaging] =
    GenPrism[InstrumentMode, GmosNorthImaging]

  val gmosSouthImaging: Prism[InstrumentMode, GmosSouthImaging] =
    GenPrism[InstrumentMode, GmosSouthImaging]

  given Encoder[InstrumentMode] = a =>
    a match
      case a @ GmosNorthSpectroscopy(_, _, _, _, _, _) =>
        Json.obj("gmosNSpectroscopy" -> a.asJson)
      case a @ GmosSouthSpectroscopy(_, _, _, _, _, _) =>
        Json.obj("gmosSSpectroscopy" -> a.asJson)
      case a @ GmosNorthImaging(_, _)                  =>
        Json.obj("gmosNImaging" -> a.asJson)
      case a @ GmosSouthImaging(_, _)                  =>
        Json.obj("gmosSImaging" -> a.asJson)
      case a @ Flamingos2Spectroscopy(_, _, _)         =>
        Json.obj("flamingos2Spectroscopy" -> a.asJson)

  given Decoder[InstrumentMode] = c =>
    for
      ns <- c.downField("gmosNSpectroscopy").as[Option[GmosNorthSpectroscopy]]
      ss <- c.downField("gmosSSpectroscopy").as[Option[GmosSouthSpectroscopy]]
      ni <- c.downField("gmosNImaging").as[Option[GmosNorthImaging]]
      si <- c.downField("gmosSImaging").as[Option[GmosSouthImaging]]
      fs <- c.downField("flamingos2Spectroscopy").as[Option[Flamingos2Spectroscopy]]
      m  <- (ns, ss, ni, si, fs) match
              case (Some(n), None, None, None, None) => (n: InstrumentMode).asRight
              case (None, Some(s), None, None, None) => (s: InstrumentMode).asRight
              case (None, None, Some(s), None, None) => (s: InstrumentMode).asRight
              case (None, None, None, Some(s), None) => (s: InstrumentMode).asRight
              case (None, None, None, None, Some(s)) => (s: InstrumentMode).asRight
              case _                                 =>
                DecodingFailure("Expected exactly one of 'gmosN' or 'gmosS' or 'flamingos2'.",
                                c.history
                ).asLeft
    yield m

  given Eq[InstrumentMode] with
    def eqv(x: InstrumentMode, y: InstrumentMode): Boolean =
      (x, y) match
        case (x0: GmosNorthSpectroscopy, y0: GmosNorthSpectroscopy)   => x0 === y0
        case (x0: GmosSouthSpectroscopy, y0: GmosSouthSpectroscopy)   => x0 === y0
        case (x0: GmosNorthImaging, y0: GmosNorthImaging)             => x0 === y0
        case (x0: GmosSouthImaging, y0: GmosSouthImaging)             => x0 === y0
        case (x0: Flamingos2Spectroscopy, y0: Flamingos2Spectroscopy) => x0 === y0
        case _                                                        => false
}
