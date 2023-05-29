// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import buildinfo.BuildInfo
import cats._
import cats.data.*
import cats.derived.*
import cats.effect.*
import cats.syntax.all.*
import dev.profunktor.redis4cats.algebra.StringCommands
import edu.gemini.grackle.*
import edu.gemini.grackle.circe.CirceMapping
import eu.timepit.refined.*
import eu.timepit.refined.numeric.NonNegative
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.syntax.*
import lucuma.core.enums.*
import lucuma.core.math.RadialVelocity
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.model.NonNegDuration
import lucuma.core.model.SourceProfile
import lucuma.core.util.TimeSpan
import lucuma.itc.ItcVersions
import lucuma.itc.SpectroscopyGraphResult
import lucuma.itc.*
import lucuma.itc.encoders.given
import lucuma.itc.search.ItcObservationDetails
import lucuma.itc.search.ObservingMode
import lucuma.itc.search.TargetProfile
import lucuma.itc.search.hashes.given
import lucuma.itc.service.config.*
import lucuma.itc.service.syntax.all.*
import natchez.Trace
import org.typelevel.log4cats.Logger

import java.time.Duration
import scala.concurrent.duration.*
import scala.io.Source
import scala.util.Using

import Query.*
import Value.*
import QueryCompiler.*

case class GraphRequest(
  targetProfile:   TargetProfile,
  specMode:        ObservingMode.SpectroscopyMode,
  constraints:     ItcObservingConditions,
  expTime:         NonNegDuration,
  exp:             PosInt,
  signalToNoiseAt: Option[Wavelength]
) derives Hash

case class SpectroscopyIntegrationTimeRequest(
  targetProfile:   TargetProfile,
  specMode:        ObservingMode.SpectroscopyMode,
  constraints:     ItcObservingConditions,
  signalToNoise:   SignalToNoise,
  signalToNoiseAt: Option[Wavelength]
) derives Hash

case class ImagingIntegrationTimeRequest(
  targetProfile: TargetProfile,
  specMode:      ObservingMode.ImagingMode,
  constraints:   ItcObservingConditions,
  signalToNoise: SignalToNoise
) derives Hash

object ItcMapping extends ItcCacheOrRemote with Version with GracklePartials {

  // In principle this is a pure operation because resources are constant values, but the potential
  // for error in dev is high and it's nice to handle failures in `F`.
  def loadSchema[F[_]: Sync: Logger]: F[Schema] =
    Sync[F]
      .defer {
        Using(Source.fromResource("graphql/itc.graphql", getClass().getClassLoader())) { src =>
          Schema(src.mkString).right.get
        }.liftTo[F]
      }

  def calculateImagingIntegrationTime[F[_]: MonadThrow: Logger: Parallel: Trace: Clock](
    environment: ExecutionEnvironment,
    redis:       StringCommands[F, Array[Byte], Array[Byte]],
    itc:         Itc[F]
  )(env: Cursor.Env): F[Result[IntegrationTimeCalculationResult]] =
    (env.get[Wavelength]("wavelength"),
     env.get[RadialVelocity]("radialVelocity").flatMap(_.toRedshift),
     env.get[SignalToNoise]("signalToNoise"),
     env.get[SourceProfile]("sourceProfile"),
     env.get[ImagingParams]("mode"),
     env.get[Band]("band"),
     env.get[ItcObservingConditions]("constraints")
    ).traverseN { (wv, rs, sn, sp, mode, band, c) =>
      val imgMode = mode match {
        case GmosNImagingParams(filter) =>
          ObservingMode.ImagingMode.GmosNorth(wv, filter)
        case GmosSImagingParams(filter) =>
          ObservingMode.ImagingMode.GmosSouth(wv, filter)
      }
      imgTimeFromCacheOrRemote(
        ImagingIntegrationTimeRequest(
          TargetProfile(sp, band, rs),
          imgMode,
          c,
          sn
        )
      )(itc, redis).map { r =>
        IntegrationTimeCalculationResult(
          version(environment).value,
          BuildInfo.ocslibHash,
          imgMode,
          r
        ).rightIorNec
      }
    }.map(
      _.getOrElse(
        Problem(s"Missing parameters for imagingIntegrationTime").leftIorNec
      )
    )

  def calculateSpectroscopyIntegrationTime[F[_]: MonadThrow: Logger: Parallel: Trace: Clock](
    environment: ExecutionEnvironment,
    redis:       StringCommands[F, Array[Byte], Array[Byte]],
    itc:         Itc[F]
  )(env: Cursor.Env): F[Result[IntegrationTimeCalculationResult]] =
    (env.get[Wavelength]("wavelength"),
     env.get[RadialVelocity]("radialVelocity").flatMap(_.toRedshift),
     env.get[SignalToNoise]("signalToNoise"),
     env.get[SourceProfile]("sourceProfile"),
     env.get[Band]("band"),
     env.get[SpectroscopyParams]("mode"),
     env.get[ItcObservingConditions]("constraints")
    ).traverseN { (wv, rs, sn, sp, sd, mode, c) =>
      {
        val signalToNoiseAt = env.get[Wavelength]("signalToNoiseAt")
        Logger[F].info(
          s"ITC calculate for $mode, conditions $c and profile $sp, at $signalToNoiseAt"
        ) *> {
          val specMode = mode match {
            case GmosNSpectroscopyParams(grating, fpu, filter) =>
              ObservingMode.SpectroscopyMode.GmosNorth(wv, grating, fpu, filter)
            case GmosSSpectroscopyParams(grating, fpu, filter) =>
              ObservingMode.SpectroscopyMode.GmosSouth(wv, grating, fpu, filter)
          }
          specTimeFromCacheOrRemote(
            SpectroscopyIntegrationTimeRequest(
              TargetProfile(sp, sd, rs),
              specMode,
              c,
              sn,
              signalToNoiseAt
            )
          )(itc, redis)
            .map { r =>
              IntegrationTimeCalculationResult(version(environment).value,
                                               BuildInfo.ocslibHash,
                                               specMode,
                                               r
              )
            }
        }
      }
        .map(_.rightIor[NonEmptyChain[Problem]])
        .handleError {
          case x: IntegrationTimeError =>
            Problem(x.message).leftIorNec
          case x                       =>
            Problem(s"Error calculating itc $x").leftIorNec
        }
    }.map(
      _.getOrElse(
        Problem(s"Missing parameters for spectroscopyIntegrationTime $env").leftIorNec
      )
    )

  def spectroscopyIntegrationTimeAndGraph[F[_]: MonadThrow: Logger: Parallel: Trace: Clock](
    environment: ExecutionEnvironment,
    redis:       StringCommands[F, Array[Byte], Array[Byte]],
    itc:         Itc[F]
  )(env: Cursor.Env): F[Result[SpectroscopyTimeAndGraphResult]] =
    (for {
      time         <- IorT(calculateSpectroscopyIntegrationTime(environment, redis, itc)(env))
      expTime       = time.results.head.exposureTime
      exposures     = time.results.head.exposures
      exposureTime <-
        IorT
          .fromEither[F](
            NonNegDuration
              .from(expTime.toDuration)
          )
          .leftMap(u => NonEmptyChain(Problem(u))): IorT[F, NonEmptyChain[Problem], NonNegDuration]
      graphEnv      =
        env.add("exposureTime" -> exposureTime).add("exposures" -> exposures)
      graph        <- IorT(spectroscopyGraph(environment, redis, itc)(graphEnv))
    } yield SpectroscopyTimeAndGraphResult.fromTimeAndGraph(expTime, exposures, graph)).value

  def spectroscopyGraph[F[_]: MonadThrow: Logger: Parallel: Trace: Clock](
    environment: ExecutionEnvironment,
    redis:       StringCommands[F, Array[Byte], Array[Byte]],
    itc:         Itc[F]
  )(env: Cursor.Env): F[Result[SpectroscopyGraphResult]] =
    (env.get[Wavelength]("wavelength"),
     env.get[RadialVelocity]("radialVelocity").flatMap(_.toRedshift),
     env.get[NonNegDuration]("exposureTime"),
     env.get[PosInt]("exposures"),
     env.get[SourceProfile]("sourceProfile"),
     env.get[Band]("band"),
     env.get[SpectroscopyParams]("mode"),
     env.get[ItcObservingConditions]("constraints")
    ).traverseN { (wv, rs, expTime, exp, sp, sd, mode, c) =>
      Logger[F].info(
        s"ITC graph calculate for $mode, conditions $c, exposureTime $expTime x $exp and profile $sp"
      ) *> {
        val signalToNoiseAt    = env.get[Wavelength]("signalToNoiseAt")
        val significantFigures = env.get[SignificantFigures]("significantFigures")
        val specMode           = mode match {
          case GmosNSpectroscopyParams(grating, fpu, filter) =>
            ObservingMode.SpectroscopyMode.GmosNorth(wv, grating, fpu, filter)
          case GmosSSpectroscopyParams(grating, fpu, filter) =>
            ObservingMode.SpectroscopyMode.GmosSouth(wv, grating, fpu, filter)
        }
        graphFromCacheOrRemote(
          GraphRequest(TargetProfile(sp, sd, rs), specMode, c, expTime, exp, signalToNoiseAt)
        )(itc, redis)
          .map { r =>
            val charts                              =
              significantFigures.fold(r.charts)(v => r.charts.map(_.adjustSignificantFigures(v)))
            val ccds                                =
              significantFigures.fold(r.ccds)(v => r.ccds.map(_.adjustSignificantFigures(v)))
            val peakFinalSNRatio                    =
              significantFigures.fold(r.peakFinalSNRatio)(
                r.peakFinalSNRatio.adjustSignificantFigures
              )
            val peakSingleSNRatio                   =
              significantFigures.fold(r.peakSingleSNRatio)(
                r.peakSingleSNRatio.adjustSignificantFigures
              )
            val atWvFinalSNRatio: Option[FinalSN]   =
              significantFigures.fold(r.atWavelengthFinalSNRatio)(s =>
                r.atWavelengthFinalSNRatio.map(_.adjustSignificantFigures(s))
              )
            val atWvSingleSNRatio: Option[SingleSN] =
              significantFigures.fold(r.atWavelengthSingleSNRatio)(s =>
                r.atWavelengthSingleSNRatio.map(_.adjustSignificantFigures(s))
              )
            SpectroscopyGraphResult(version(environment).value,
                                    BuildInfo.ocslibHash,
                                    ccds,
                                    charts.flatMap(_.charts),
                                    peakFinalSNRatio,
                                    atWvFinalSNRatio,
                                    peakSingleSNRatio,
                                    atWvSingleSNRatio
            )
          }
      }
        .map(_.rightIor[NonEmptyChain[Problem]])
        .handleError {
          case x: IntegrationTimeError =>
            Problem(x.message).leftIorNec
          case x                       =>
            Problem(s"Error calculating itc $x").leftIorNec
        }
    }.map(
      _.getOrElse(
        Problem(s"Missing parameters for calculateSpectroscopyGraph $env").leftIorNec
      )
    )

  def calculateSignalToNoise[F[_]: MonadThrow: Logger: Parallel: Trace: Clock](
    environment: ExecutionEnvironment,
    redis:       StringCommands[F, Array[Byte], Array[Byte]],
    itc:         Itc[F]
  )(env: Cursor.Env): F[Result[SNCalcResult]] =
    (env.get[Wavelength]("wavelength"),
     env.get[RadialVelocity]("radialVelocity").flatMap(_.toRedshift),
     env.get[NonNegDuration]("exposureTime"),
     env.get[PosInt]("exposures"),
     env.get[SourceProfile]("sourceProfile"),
     env.get[Band]("band"),
     env.get[SpectroscopyParams]("mode"),
     env.get[ItcObservingConditions]("constraints")
    ).traverseN { (wv, rs, expTime, exp, sp, sd, mode, c) =>
      Logger[F].info(
        s"ITC sn calculation for $mode, conditions $c, exposureTime $expTime x $exp and profile $sp"
      ) *> {
        val significantFigures = env.get[SignificantFigures]("significantFigures")
        val signalToNoiseAt    = env.get[Wavelength]("signalToNoiseAt")
        val specMode           = mode match {
          case GmosNSpectroscopyParams(grating, fpu, filter) =>
            ObservingMode.SpectroscopyMode.GmosNorth(wv, grating, fpu, filter)
          case GmosSSpectroscopyParams(grating, fpu, filter) =>
            ObservingMode.SpectroscopyMode.GmosSouth(wv, grating, fpu, filter)
        }
        import io.circe.syntax.*

        graphFromCacheOrRemote(
          GraphRequest(TargetProfile(sp, sd, rs), specMode, c, expTime, exp, signalToNoiseAt)
        )(itc, redis)
          .flatMap { r =>
            itc.calculateSignalToNoise(r.charts, signalToNoiseAt)
          }
      }
        .map(_.rightIor[NonEmptyChain[Problem]])
        .handleError {
          case x: IntegrationTimeError =>
            Problem(x.message).leftIorNec
          case x                       =>
            Problem(s"Error calculating itc $x").leftIorNec
        }
    }.map(
      _.getOrElse(Problem(s"Missing parameters for signal to noise calculation$env").leftIorNec)
    )

  def versions[F[_]: Applicative: Logger](
    environment: ExecutionEnvironment,
    redis:       StringCommands[F, Array[Byte], Array[Byte]]
  ): F[Result[ItcVersions]] =
    ItcVersions(version(environment).value, BuildInfo.ocslibHash.some)
      .rightIor[NonEmptyChain[Problem]]
      .pure[F]

  def apply[F[_]: Sync: Logger: Parallel: Trace](
    environment: ExecutionEnvironment,
    redis:       StringCommands[F, Array[Byte], Array[Byte]],
    itc:         Itc[F]
  ): F[Mapping[F]] =
    loadSchema[F].map { loadedSchema =>
      new CirceMapping[F] {

        val schema: Schema    = loadedSchema
        val QueryType         = schema.ref("Query")
        val BigDecimalType    = schema.ref("BigDecimal")
        val LongType          = schema.ref("Long")
        val PosIntType        = schema.ref("PosInt")
        val SignalToNoiseType = schema.ref("SignalToNoise")

        val typeMappings =
          List(
            ObjectMapping(
              tpe = QueryType,
              fieldMappings = List(
                RootEffect.computeEncodable("versions")((_, p, env) =>
                  versions(environment, redis)
                ),
                RootEffect.computeEncodable("spectroscopyIntegrationTime") { (_, p, env) =>
                  calculateSpectroscopyIntegrationTime(environment, redis, itc)(env)
                },
                RootEffect.computeEncodable("spectroscopySignalToNoise")((_, p, env) =>
                  calculateSignalToNoise(environment, redis, itc)(env)
                ),
                RootEffect.computeEncodable("optimizedSpectroscopyGraph")((_, p, env) =>
                  spectroscopyGraph(environment, redis, itc)(env)
                ),
                RootEffect.computeEncodable("imagingIntegrationTime")((_, p, env) =>
                  calculateImagingIntegrationTime(environment, redis, itc)(env)
                ),
                RootEffect.computeEncodable("spectroscopyIntegrationTimeAndGraph")((_, p, env) =>
                  spectroscopyIntegrationTimeAndGraph(environment, redis, itc)(env)
                )
              )
            ),
            LeafMapping[BigDecimal](BigDecimalType),
            LeafMapping[Long](LongType),
            LeafMapping[PosInt](PosIntType),
            LeafMapping[SignalToNoise](SignalToNoiseType)
          )

        def fallback(a: (IorNec[Problem, Environment], (String, Value))) =
          a._1.addProblem(s"Unexpected param ${a._2._1}")

        override val selectElaborator =
          new SelectElaborator(
            Map(
              QueryType -> {
                case Select("spectroscopyIntegrationTime",
                            List(Binding("input", ObjectValue(wv))),
                            child
                    ) =>
                  wv.foldLeft(Environment(Cursor.Env(), child).rightIor[NonEmptyChain[Problem]]) {
                    case (e, c) =>
                      wavelengthPartial
                        .orElse(radialVelocityPartial)
                        .orElse(signalToNoisePartial)
                        .orElse(sourceProfilePartial)
                        .orElse(bandPartial)
                        .orElse(instrumentModePartial)
                        .orElse(constraintsPartial)
                        .orElse(signalToNoiseAtPartial)
                        .applyOrElse(
                          (e, c),
                          fallback
                        )
                  }.map(e => e.copy(child = Select("spectroscopyIntegrationTime", Nil, child)))

                case Select("imagingIntegrationTime",
                            List(Binding("input", ObjectValue(wv))),
                            child
                    ) =>
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
                  }.map(e => e.copy(child = Select("imagingIntegrationTime", Nil, child)))

                case Select("optimizedSpectroscopyGraph",
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
                        .orElse(signalToNoiseAtPartial)
                        .applyOrElse(
                          (e, c),
                          fallback
                        )
                  }.map(e => e.copy(child = Select("optimizedSpectroscopyGraph", Nil, child)))

                case Select("spectroscopyIntegrationTimeAndGraph",
                            List(Binding("input", ObjectValue(wv))),
                            child
                    ) =>
                  wv.foldLeft(Environment(Cursor.Env(), child).rightIor[NonEmptyChain[Problem]]) {
                    case (e, c) =>
                      wavelengthPartial
                        .orElse(signalToNoisePartial)
                        .orElse(signalToNoiseAtPartial)
                        .orElse(sourceProfilePartial)
                        .orElse(bandPartial)
                        .orElse(radialVelocityPartial)
                        .orElse(constraintsPartial)
                        .orElse(instrumentModePartial)
                        .orElse(significantFiguresPartial)
                        .applyOrElse(
                          (e, c),
                          fallback
                        )
                  }.map(e =>
                    e.copy(child = Select("spectroscopyIntegrationTimeAndGraph", Nil, child))
                  )
                case Select("spectroscopySignalToNoise",
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
                        .orElse(signalToNoiseAtPartial)
                        .applyOrElse(
                          (e, c),
                          fallback
                        )
                  }.map(e => e.copy(child = Select("spectroscopySignalToNoise", Nil, child)))
              }
            )
          )
      }
    }
}
