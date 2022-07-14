// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import algebra.instances.all.given
import buildinfo.BuildInfo
import cats._
import cats.data._
import cats.effect._
import cats.syntax.all._
import coulomb.*
import coulomb.ops.algebra.spire.all.given
import coulomb.policy.spire.standard.given
import coulomb.syntax.*
import coulomb.units.si.*
import coulomb.units.si.given
import coulomb.units.si.prefixes.*
import coulomb.units.time.*
import edu.gemini.grackle._
import edu.gemini.grackle.circe.CirceMapping
import eu.timepit.refined._
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Encoder
import io.circe.Json
import lucuma.core.enums._
import lucuma.core.math.Angle
import lucuma.core.math.BrightnessUnits._
import lucuma.core.math.RadialVelocity
import lucuma.core.math.Wavelength
import lucuma.core.math.dimensional.Units._
import lucuma.core.math.dimensional._
import lucuma.core.math.units._
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.UnnormalizedSED
import lucuma.core.model.UnnormalizedSED._
import lucuma.core.syntax.enumerated._
import lucuma.core.syntax.string._
import lucuma.core.util.Enumerated
import lucuma.itc.GmosNITCParams
import lucuma.itc.GmosSITCParams
import lucuma.itc.Itc
import lucuma.itc.ItcObservingConditions
import lucuma.itc.SpectroscopyParams
import lucuma.itc.UpstreamException
import lucuma.itc.given
import lucuma.itc.search.GmosNorthFpuParam
import lucuma.itc.search.GmosSouthFpuParam
import lucuma.itc.search.ObservingMode
import lucuma.itc.search.ObservingMode.Spectroscopy.GmosNorth
import lucuma.itc.search.ObservingMode.Spectroscopy.GmosSouth
import lucuma.itc.search.Result.Spectroscopy
import lucuma.itc.search.SpectroscopyResults
import lucuma.itc.search.TargetProfile
import lucuma.itc.service.config._
import lucuma.itc.service.syntax.all._
import natchez.Trace
import org.typelevel.log4cats.Logger

import java.math.RoundingMode
import java.time.Instant
import java.time.ZoneId
import java.time.ZoneOffset
import java.time.format.DateTimeFormatter
import scala.collection.immutable.SortedMap
import scala.concurrent.duration._
import scala.io.Source
import scala.util.Using

import Query._
import Value._
import QueryCompiler._

object ItcMapping extends Version with GracklePartials {

  // In principle this is a pure operation because resources are constant values, but the potential
  // for error in dev is high and it's nice to handle failures in `F`.
  def loadSchema[F[_]: Sync]: F[Schema] =
    Sync[F]
      .defer {
        Using(Source.fromResource("graphql/itc.graphql", getClass().getClassLoader())) { src =>
          Schema(src.mkString).right.get
        }.liftTo[F]
      }

  def spectroscopy[F[_]: ApplicativeThrow: Logger: Parallel: Trace](
    environment: ExecutionEnvironment,
    itc:         Itc[F]
  )(env:         Cursor.Env): F[Result[List[SpectroscopyResults]]] =
    (env.get[Wavelength]("wavelength"),
     env.get[RadialVelocity]("radialVelocity").flatMap(_.toRedshift),
     env.get[PosBigDecimal]("signalToNoise"),
     env.get[SourceProfile]("sourceProfile"),
     env.get[Band]("band"),
     env.get[List[SpectroscopyParams]]("modes"),
     env.get[ItcObservingConditions]("constraints")
    ).traverseN { (wv, rs, sn, sp, sd, modes, c) =>
      modes
        .parTraverse { mode =>
          Logger[F].info(s"ITC calculate for $mode, conditions $c and profile $sp") *>
            Trace[F].put(("itc.modes_count", modes.length)) *> {
              val specMode = mode match {
                case GmosNITCParams(grating, fpu, filter) =>
                  ObservingMode.Spectroscopy.GmosNorth(wv, grating, fpu, filter)
                case GmosSITCParams(grating, fpu, filter) =>
                  ObservingMode.Spectroscopy.GmosSouth(wv, grating, fpu, filter)
              }
              itc
                .calculate(
                  TargetProfile(sp, sd, rs),
                  specMode,
                  c,
                  sn.value
                )
                .handleErrorWith {
                  case UpstreamException(msg) =>
                    Itc.Result.CalculationError(msg).pure[F].widen
                  case x                      =>
                    Itc.Result.CalculationError(s"Error calculating itc $x").pure[F].widen
                }
                .map(r =>
                  SpectroscopyResults(version(environment).value, List(Spectroscopy(specMode, r)))
                )
            }
        }
        .map(_.rightIor[NonEmptyChain[Problem]])
        .handleErrorWith { case x =>
          Problem(s"Error calculating itc $x").leftIorNec.pure[F]
        }
    }.map(_.getOrElse(Problem("Missing parameters for spectroscopy").leftIorNec))

  def spectroscopyGraph[F[_]: ApplicativeThrow: Logger: Parallel: Trace](
    environment: ExecutionEnvironment,
    itc:         Itc[F]
  )(env:         Cursor.Env): F[Result[List[SpectroscopyResults]]] =
    (env.get[Wavelength]("wavelength"),
     env.get[RadialVelocity]("radialVelocity").flatMap(_.toRedshift),
     env.get[PosBigDecimal]("signalToNoise"),
     env.get[SourceProfile]("sourceProfile"),
     env.get[Band]("band"),
     env.get[SpectroscopyParams]("mode"),
     env.get[ItcObservingConditions]("constraints")
    ).traverseN { (wv, rs, sn, sp, sd, modes, c) =>
      List(modes)
        .parTraverse { mode =>
          Logger[F].info(s"ITC calculate for $mode, conditions $c and profile $sp") *> {
            val specMode = mode match {
              case GmosNITCParams(grating, fpu, filter) =>
                ObservingMode.Spectroscopy.GmosNorth(wv, grating, fpu, filter)
              case GmosSITCParams(grating, fpu, filter) =>
                ObservingMode.Spectroscopy.GmosSouth(wv, grating, fpu, filter)
            }
            itc
              .calculate(
                TargetProfile(sp, sd, rs),
                specMode,
                c,
                sn.value
              )
              .handleErrorWith {
                case UpstreamException(msg) =>
                  Itc.Result.CalculationError(msg).pure[F].widen
                case x                      =>
                  Itc.Result.CalculationError(s"Error calculating itc $x").pure[F].widen
              }
              .map(r =>
                SpectroscopyResults(version(environment).value, List(Spectroscopy(specMode, r)))
              )
          }
        }
        .map(_.rightIor[NonEmptyChain[Problem]])
        .handleErrorWith { case x =>
          Problem(s"Error calculating itc $x").leftIorNec.pure[F]
        }
    }.map(_.getOrElse(Problem(s"Missing parameters for spectroscopy graph $env").leftIorNec))

  def apply[F[_]: Sync: Logger: Parallel: Trace](
    environment: ExecutionEnvironment,
    itc:         Itc[F]
  ): F[Mapping[F]] =
    loadSchema[F].map { loadedSchema =>
      new CirceMapping[F] with ComputeMapping[F] {

        val schema: Schema         = loadedSchema
        val QueryType              = schema.ref("Query")
        val SpectroscopyResultType = schema.ref("SpectroscopyResult")
        val BigDecimalType         = schema.ref("BigDecimal")
        val LongType               = schema.ref("Long")
        val DurationType           = schema.ref("Duration")

        val typeMappings =
          List(
            ObjectMapping(
              tpe = QueryType,
              fieldMappings = List(
                ComputeRoot[List[SpectroscopyResults]]("spectroscopy",
                                                       ListType(SpectroscopyResultType),
                                                       spectroscopy[F](environment, itc)
                ),
                ComputeRoot[List[SpectroscopyResults]]("spectroscopyGraph",
                                                       ListType(SpectroscopyResultType),
                                                       spectroscopyGraph[F](environment, itc)
                )
              )
            ),
            LeafMapping[BigDecimal](BigDecimalType),
            LeafMapping[Long](LongType),
            LeafMapping[FiniteDuration](DurationType)
          )

        def fallback(a: (IorNec[Problem, Environment], (String, Value))) =
          a._1.addProblem(s"Unexpected param ${a._2._1}")

        override val selectElaborator =
          new SelectElaborator(
            Map(
              QueryType -> {
                case Select("spectroscopy", List(Binding("input", ObjectValue(wv))), child)      =>
                  wv.foldLeft(Environment(Cursor.Env(), child).rightIor[NonEmptyChain[Problem]]) {
                    case (e, c) =>
                      wavelengthPartial
                        .orElse(radialVelocityPartial)
                        .orElse(signalToNoisePartial)
                        .orElse(sourceProfilePartial)
                        .orElse(bandPartial)
                        .orElse(instrumentModesPartial)
                        .orElse(constraintsPartial)
                        .applyOrElse(
                          (e, c),
                          fallback
                        )
                  }.map(e => e.copy(child = Select("spectroscopy", Nil, child)))
                case Select("spectroscopyGraph", List(Binding("input", ObjectValue(wv))), child) =>
                  wv.foldLeft(Environment(Cursor.Env(), child).rightIor[NonEmptyChain[Problem]]) {
                    case (e, c) =>
                      wavelengthPartial
                        .orElse(radialVelocityPartial)
                        .orElse(signalToNoisePartial)
                        .orElse(sourceProfilePartial)
                        .orElse(bandPartial)
                        .orElse(instrumentModePartial)
                        .orElse(constraintsPartial)
                        .applyOrElse(
                          (e, c),
                          fallback
                        )
                  }.map(e => e.copy(child = Select("spectroscopyGraph", Nil, child)))
              }
            )
          )
      }
    }
}
