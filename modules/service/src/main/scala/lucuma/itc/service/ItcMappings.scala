// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import algebra.instances.all.given
import buildinfo.BuildInfo
import cats._
import cats.data._
import cats.effect._
import cats.syntax.all._
import cats.derived.*
import coulomb.*
import coulomb.ops.algebra.spire.all.given
import coulomb.policy.spire.standard.given
import coulomb.syntax.*
import coulomb.units.si.*
import coulomb.units.si.given
import coulomb.units.si.prefixes.*
import coulomb.units.time.*
import dev.profunktor.redis4cats.algebra.StringCommands
import edu.gemini.grackle._
import edu.gemini.grackle.circe.CirceMapping
import eu.timepit.refined._
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.numeric.PosLong
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Encoder
import io.circe.parser.decode
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums._
import lucuma.core.math.Angle
import lucuma.core.math.BrightnessUnits._
import lucuma.core.math.RadialVelocity
import lucuma.core.math.Wavelength
import lucuma.core.math.dimensional.Units._
import lucuma.core.math.dimensional._
import lucuma.core.math.units._
import lucuma.core.model.NonNegDuration
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
import lucuma.itc.SignificantFigures
import lucuma.itc.SpectroscopyParams
import lucuma.itc.UpstreamException
import lucuma.itc.encoders.given
import lucuma.itc.search.GmosNorthFpuParam
import lucuma.itc.search.GmosSouthFpuParam
import lucuma.itc.search.ItcVersions
import lucuma.itc.search.ObservingMode
import lucuma.itc.search.ObservingMode.Spectroscopy.GmosNorth
import lucuma.itc.search.ObservingMode.Spectroscopy.GmosSouth
import lucuma.itc.search.Result.Spectroscopy
import lucuma.itc.search.SpectroscopyGraphResults
import lucuma.itc.search.SpectroscopyResults
import lucuma.itc.search.TargetProfile
import lucuma.itc.search.hashes.given
import lucuma.itc.service.config._
import lucuma.itc.service.syntax.all._
import natchez.Trace
import org.typelevel.log4cats.Logger

import java.math.RoundingMode
import java.time.Duration
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

case class GraphRequest(
  targetProfile: TargetProfile,
  specMode:      ObservingMode.Spectroscopy,
  constraints:   ItcObservingConditions,
  expTime:       NonNegDuration,
  exp:           PosLong
) derives Hash

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
                    (none, Itc.Result.CalculationError(msg)).pure[F].widen
                  case x                      =>
                    (none, Itc.Result.CalculationError(s"Error calculating itc $x")).pure[F].widen
                }
                .map { (dataVersion, r) =>
                  SpectroscopyResults(version(environment).value,
                                      dataVersion,
                                      List(Spectroscopy(specMode, r))
                  )
                }
            }
        }
        .map(_.rightIor[NonEmptyChain[Problem]])
        .handleErrorWith { case x =>
          Problem(s"Error calculating itc $x").leftIorNec.pure[F]
        }
    }.map(_.getOrElse(Problem("Missing parameters for spectroscopy").leftIorNec))

  private def calculateGraph[F[_]: Monad](
    request: GraphRequest
  )(itc:     Itc[F], redis: StringCommands[F, String, String]): F[Itc.GraphResult] =
    import lucuma.itc.redis.given
    val hash = Hash[GraphRequest].hash(request)
    for
      fromRedis <- redis.get(s"itc:graph:$hash")
      decoded   <-
        fromRedis.flatMap(decode[Itc.GraphResult](_).toOption).pure[F]
      r         <-
        decoded
          .map(_.pure[F])
          .getOrElse(
            itc
              .calculateGraph(
                request.targetProfile,
                request.specMode,
                request.constraints,
                request.expTime,
                request.exp
              )
          )
      _         <- {
        println(
          r.asJson.hcursor
            .downField("charts")
            .downArray
            .downField("charts")
            .downArray
            .downField("series")
            .downArray
            .downField("dataX")
            .focus
            .map(_.spaces2)
        )
        redis.set(s"itc:graph:$hash", r.asJson.noSpaces).whenA(fromRedis.isEmpty)
      }
    yield r

  def spectroscopyGraph[F[_]: MonadThrow: Logger: Parallel: Trace](
    environment: ExecutionEnvironment,
    redis:       StringCommands[F, String, String],
    itc:         Itc[F]
  )(env:         Cursor.Env): F[Result[SpectroscopyGraphResults]] =
    (env.get[Wavelength]("wavelength"),
     env.get[RadialVelocity]("radialVelocity").flatMap(_.toRedshift),
     env.get[NonNegDuration]("exposureTime"),
     env.get[PosLong]("exposures"),
     env.get[SourceProfile]("sourceProfile"),
     env.get[Band]("band"),
     env.get[SpectroscopyParams]("mode"),
     env.get[ItcObservingConditions]("constraints")
    ).traverseN { (wv, rs, expTime, exp, sp, sd, mode, c) =>
      Logger[F].info(
        s"ITC graph calculate for $mode, conditions $c, exposureTime $expTime x $exp and profile $sp"
      ) *> {
        val significantFigures = env.get[SignificantFigures]("significantFigures")
        val specMode           = mode match {
          case GmosNITCParams(grating, fpu, filter) =>
            ObservingMode.Spectroscopy.GmosNorth(wv, grating, fpu, filter)
          case GmosSITCParams(grating, fpu, filter) =>
            ObservingMode.Spectroscopy.GmosSouth(wv, grating, fpu, filter)
        }
        calculateGraph(
          GraphRequest(TargetProfile(sp, sd, rs), specMode, c, expTime, exp)
        )(itc, redis)
          .map { r =>
            val charts =
              significantFigures.fold(r.charts)(v => r.charts.map(_.adjustSignificantFigures(v)))
            val ccds   =
              significantFigures.fold(r.ccds)(v => r.ccds.map(_.adjustSignificantFigures(v)))
            SpectroscopyGraphResults(version(environment).value,
                                     r.dataVersion.some,
                                     ccds,
                                     charts.flatMap(_.charts)
            )
          }
      }
        .map(_.rightIor[NonEmptyChain[Problem]])
        .handleErrorWith { case x =>
          Problem(s"Error calculating itc $x").leftIorNec.pure[F]
        }
    }.map(_.getOrElse(Problem(s"Missing parameters for spectroscopy graph $env").leftIorNec))

  def versions[F[_]: MonadThrow](
    environment: ExecutionEnvironment,
    redis:       StringCommands[F, String, String],
    itc:         Itc[F]
  )(
    env:         Cursor.Env
  ): F[Result[ItcVersions]] =
    for
      fromRedis <- redis.get("itc:version")
      version   <- fromRedis.fold(
                     itc.itcVersions
                       .map { r =>
                         ItcVersions(version(environment).value, r.some)
                       }
                   )(v => ItcVersions(version(environment).value, v.some).pure[F])
      _         <- redis.set("itc:version", version.dataVersion.orEmpty).whenA(fromRedis.isEmpty)
    yield version
      .rightIor[NonEmptyChain[Problem]]
      .handleErrorWith { case x =>
        Problem(s"Error getting the itc version $x").leftIorNec
      }

  def apply[F[_]: Sync: Logger: Parallel: Trace](
    environment: ExecutionEnvironment,
    redis:       StringCommands[F, String, String],
    itc:         Itc[F]
  ): F[Mapping[F]] =
    loadSchema[F].map { loadedSchema =>
      new CirceMapping[F] with ComputeMapping[F] {

        val schema: Schema = loadedSchema
        val QueryType      = schema.ref("Query")
        val BigDecimalType = schema.ref("BigDecimal")
        val LongType       = schema.ref("Long")
        val DurationType   = schema.ref("Duration")
        val PosIntType     = schema.ref("PosInt")

        val typeMappings =
          List(
            ObjectMapping(
              tpe = QueryType,
              fieldMappings = List(
                ComputeRoot[List[SpectroscopyResults]]("spectroscopy",
                                                       QueryType,
                                                       spectroscopy[F](environment, itc)
                ),
                ComputeRoot[SpectroscopyGraphResults]("spectroscopyGraphBeta",
                                                      QueryType,
                                                      spectroscopyGraph[F](environment, redis, itc)
                ),
                ComputeRoot[ItcVersions]("versions",
                                         QueryType,
                                         versions[F](environment, redis, itc)
                )
              )
            ),
            LeafMapping[BigDecimal](BigDecimalType),
            LeafMapping[Long](LongType),
            LeafMapping[PosInt](PosIntType),
            LeafMapping[FiniteDuration](DurationType)
          )

        def fallback(a: (IorNec[Problem, Environment], (String, Value))) =
          a._1.addProblem(s"Unexpected param ${a._2._1}")

        override val selectElaborator =
          new SelectElaborator(
            Map(
              QueryType -> {
                case Select("spectroscopy", List(Binding("input", ObjectValue(wv))), child) =>
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
                case Select("spectroscopyGraphBeta",
                            List(Binding("input", ObjectValue(wv))),
                            child
                    ) =>
                  wv.foldLeft(Environment(Cursor.Env(), child).rightIor[NonEmptyChain[Problem]]) {
                    case (e, c) =>
                      wavelengthPartial
                        .orElse(radialVelocityPartial)
                        .orElse(exposureTimePartial)
                        .orElse(exposuresPartial)
                        .orElse(sourceProfilePartial)
                        .orElse(bandPartial)
                        .orElse(instrumentModePartial)
                        .orElse(constraintsPartial)
                        .orElse(significantFiguresPartial)
                        .applyOrElse(
                          (e, c),
                          fallback
                        )
                  }.map(e => e.copy(child = Select("spectroscopyGraphBeta", Nil, child)))
              }
            )
          )
      }
    }
}
