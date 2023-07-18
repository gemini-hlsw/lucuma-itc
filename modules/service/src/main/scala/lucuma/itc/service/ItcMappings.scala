// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import buildinfo.BuildInfo
import cats._
import cats.derived.*
import cats.effect.*
import cats.syntax.all.*
import dev.profunktor.redis4cats.algebra.StringCommands
import edu.gemini.grackle.*
import edu.gemini.grackle.circe.CirceMapping
import eu.timepit.refined.*
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.*
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.util.TimeSpan
import lucuma.itc.ItcVersions
import lucuma.itc.SpectroscopyGraphResult
import lucuma.itc.*
import lucuma.itc.input.*
import lucuma.itc.encoders.given
import lucuma.itc.search.ObservingMode
import lucuma.itc.search.TargetProfile
import lucuma.itc.search.hashes.given
import lucuma.itc.service.config.*
import lucuma.itc.service.syntax.all.*
import natchez.Trace
import org.typelevel.log4cats.Logger

import scala.io.Source
import scala.util.Using

import Query.*
import QueryCompiler.*
import lucuma.core.model.sequence.gmos.GmosFpuMask
import lucuma.itc.search.GmosNorthFpuParam
import lucuma.itc.search.GmosSouthFpuParam

case class GraphRequest(
  targetProfile:      TargetProfile,
  specMode:           ObservingMode.SpectroscopyMode,
  constraints:        ItcObservingConditions,
  expTime:            TimeSpan,
  exp:                PosInt,
  signalToNoiseAt:    Option[Wavelength],
  significantFigures: Option[SignificantFigures]
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
          Schema(src.mkString).toEither.fold(
            x => sys.error(s"Invalid schema: ${x.toList.mkString(", ")}"),
            identity
          )
        }.liftTo[F]
      }

  private def toSpectroscopyTimeRequest(
    input: SpectroscopyTimeInput
  ): Result[SpectroscopyIntegrationTimeRequest] = {
    val wavelength      = input.wavelength
    val signalToNoiseAt = input.signalToNoiseAt
    val signalToNoise   = input.signalToNoise
    val sourceProfile   = input.sourceProfile
    val band            = input.band
    val radialVelocity  = input.radialVelocity
    val constraints     = input.constraints
    val mode            = input.mode

    val redshift = Result.fromOption(radialVelocity.toRedshift, "Invalid radial velocity")
    val specMode = mode match {
      case GmosNSpectroscopyInput(grating, GmosFpuMask.Builtin(fpu), filter) =>
        Result(
          ObservingMode.SpectroscopyMode
            .GmosNorth(wavelength, grating, GmosNorthFpuParam(fpu), filter)
        )
      case GmosSSpectroscopyInput(grating, GmosFpuMask.Builtin(fpu), filter) =>
        Result(
          ObservingMode.SpectroscopyMode
            .GmosSouth(wavelength, grating, GmosSouthFpuParam(fpu), filter)
        )
      case _                                                                 =>
        Result.failure("Invalid spectroscopy mode")
    }

    val itcConditions =
      constraints.create
        .flatMap(c => Result.fromEither(ItcObservingConditions.fromConstraints(c)))

    (redshift, specMode, itcConditions).parMapN { (rs, mode, conditions) =>
      SpectroscopyIntegrationTimeRequest(
        TargetProfile(sourceProfile, band, rs),
        mode,
        conditions,
        signalToNoise,
        signalToNoiseAt
      )
    }
  }

  def calculateSpectroscopyIntegrationTime[F[_]: MonadThrow: Logger: Parallel: Trace: Clock](
    environment: ExecutionEnvironment,
    redis:       StringCommands[F, Array[Byte], Array[Byte]],
    itc:         Itc[F]
  )(input: SpectroscopyIntegrationTimeRequest): F[IntegrationTimeCalculationResult] =
    specTimeFromCacheOrRemote(input)(itc, redis)
      .adaptError {
        case x: IntegrationTimeError =>
          new RuntimeException(x.message)
        case UpstreamException(msg)  =>
          new RuntimeException(msg.mkString("\n"))
        case x                       =>
          new RuntimeException(s"Error calculating itc $x")
      }
      .map { r =>
        IntegrationTimeCalculationResult(
          version(environment).value,
          BuildInfo.ocslibHash,
          input.specMode,
          r
        )
      }

  private def toImagingTimeRequest(
    input: ImagingIntegrationTimeInput
  ): Result[ImagingIntegrationTimeRequest] = {
    val ImagingIntegrationTimeInput(wavelength,
                                    signalToNoise,
                                    sourceProfile,
                                    band,
                                    radialVelocity,
                                    constraints,
                                    mode
    ) = input

    val redshift = Result.fromOption(radialVelocity.toRedshift, "Invalid radial velocity")
    val specMode = mode match {
      case GmosNImagingInput(filter) =>
        Result(ObservingMode.ImagingMode.GmosNorth(wavelength, filter))
      case GmosSImagingInput(filter) =>
        Result(ObservingMode.ImagingMode.GmosSouth(wavelength, filter))
      case _                         =>
        Result.failure("Invalid spectroscopy mode")
    }

    val itcConditions =
      constraints.create
        .flatMap(c => Result.fromEither(ItcObservingConditions.fromConstraints(c)))
    (redshift, specMode, itcConditions).parMapN { (rs, mode, conditions) =>
      ImagingIntegrationTimeRequest(
        TargetProfile(sourceProfile, band, rs),
        mode,
        conditions,
        signalToNoise
      )
    }
  }

  def calculateImagingIntegrationTime[F[_]: MonadThrow: Logger: Parallel: Trace: Clock](
    environment: ExecutionEnvironment,
    redis:       StringCommands[F, Array[Byte], Array[Byte]],
    itc:         Itc[F]
  )(input: ImagingIntegrationTimeRequest): F[IntegrationTimeCalculationResult] =
    imgTimeFromCacheOrRemote(input)(itc, redis)
      .adaptError {
        case x: IntegrationTimeError =>
          new RuntimeException(x.message)
        case UpstreamException(msg)  =>
          new RuntimeException(msg.mkString("\n"))
        case x                       =>
          new RuntimeException(s"Error calculating itc $x")
      }
      .map { r =>
        IntegrationTimeCalculationResult(
          version(environment).value,
          BuildInfo.ocslibHash,
          input.specMode,
          r
        )
      }

  private def toGraphRequest(
    input: OptimizedSpectroscopyGraphInput
  ): Result[GraphRequest] = {
    val OptimizedSpectroscopyGraphInput(wavelength,
                                        signalToNoiseAt,
                                        exposureTime,
                                        exposures,
                                        sourceProfile,
                                        band,
                                        radialVelocity,
                                        constraints,
                                        mode,
                                        figures
    ) = input

    val redshift = Result.fromOption(radialVelocity.toRedshift, "Invalid radial velocity")
    val specMode = mode match {
      case GmosNSpectroscopyInput(grating, GmosFpuMask.Builtin(fpu), filter) =>
        Result(
          ObservingMode.SpectroscopyMode
            .GmosNorth(wavelength, grating, GmosNorthFpuParam(fpu), filter)
        )
      case GmosSSpectroscopyInput(grating, GmosFpuMask.Builtin(fpu), filter) =>
        Result(
          ObservingMode.SpectroscopyMode
            .GmosSouth(wavelength, grating, GmosSouthFpuParam(fpu), filter)
        )
      case _                                                                 =>
        Result.failure("Invalid spectroscopy mode")
    }

    val itcConditions =
      constraints.create
        .flatMap(c => Result.fromEither(ItcObservingConditions.fromConstraints(c)))

    (redshift, specMode, itcConditions).parMapN { (rs, mode, conditions) =>
      GraphRequest(
        TargetProfile(sourceProfile, band, rs),
        mode,
        conditions,
        exposureTime,
        exposures,
        signalToNoiseAt,
        figures
      )
    }
  }

  def spectroscopyIntegrationTimeAndGraph[F[_]: MonadThrow: Logger: Parallel: Trace: Clock](
    environment: ExecutionEnvironment,
    redis:       StringCommands[F, Array[Byte], Array[Byte]],
    itc:         Itc[F]
  )(
    tr:          SpectroscopyIntegrationTimeRequest,
    fig:         Option[SignificantFigures]
  ): F[SpectroscopyTimeAndGraphResult] =
    for {
      specTime <-
        calculateSpectroscopyIntegrationTime(environment, redis, itc)(tr)
      expTime   = specTime.results.head.exposureTime
      exps      = specTime.results.head.exposures
      gr        = GraphRequest(tr.targetProfile,
                               tr.specMode,
                               tr.constraints,
                               expTime,
                               exps,
                               tr.signalToNoiseAt,
                               fig
                  )
      gr       <- spectroscopyGraph(environment, redis, itc)(gr)
    } yield SpectroscopyTimeAndGraphResult(
      gr.serverVersion,
      gr.dataVersion,
      expTime,
      exps,
      gr.ccds,
      gr.charts,
      gr.peakFinalSNRatio,
      gr.atWavelengthFinalSNRatio,
      gr.peakSingleSNRatio,
      gr.atWavelengthSingleSNRatio
    )

  def spectroscopyGraph[F[_]: MonadThrow: Logger: Parallel: Trace: Clock](
    environment: ExecutionEnvironment,
    redis:       StringCommands[F, Array[Byte], Array[Byte]],
    itc:         Itc[F]
  )(input: GraphRequest): F[SpectroscopyGraphResult] =
    graphFromCacheOrRemote(input)(itc, redis)
      .map { r =>
        val charts                              =
          input.significantFigures.fold(r.charts)(v => r.charts.map(_.adjustSignificantFigures(v)))
        val ccds                                =
          input.significantFigures.fold(r.ccds)(v => r.ccds.map(_.adjustSignificantFigures(v)))
        val peakFinalSNRatio                    =
          input.significantFigures.fold(r.peakFinalSNRatio)(
            r.peakFinalSNRatio.adjustSignificantFigures
          )
        val peakSingleSNRatio                   =
          input.significantFigures.fold(r.peakSingleSNRatio)(
            r.peakSingleSNRatio.adjustSignificantFigures
          )
        val atWvFinalSNRatio: Option[FinalSN]   =
          input.significantFigures.fold(r.atWavelengthFinalSNRatio)(s =>
            r.atWavelengthFinalSNRatio.map(_.adjustSignificantFigures(s))
          )
        val atWvSingleSNRatio: Option[SingleSN] =
          input.significantFigures.fold(r.atWavelengthSingleSNRatio)(s =>
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
      .adaptError {
        case x: IntegrationTimeError =>
          new RuntimeException(x.message)
        case UpstreamException(msg)  =>
          new RuntimeException(msg.mkString("\n"))
        case x                       =>
          new RuntimeException(s"Error calculating itc $x")
      }

  //
  // def calculateSignalToNoise[F[_]: MonadThrow: Logger: Parallel: Trace: Clock](
  //   environment: ExecutionEnvironment,
  //   redis:       StringCommands[F, Array[Byte], Array[Byte]],
  //   itc:         Itc[F]
  // )(env: Cursor.Env): F[Result[SNCalcResult]] =
  //   (env.get[Wavelength]("wavelength"),
  //    env.get[RadialVelocity]("radialVelocity").flatMap(_.toRedshift),
  //    env.get[NonNegDuration]("exposureTime"),
  //    env.get[PosInt]("exposures"),
  //    env.get[SourceProfile]("sourceProfile"),
  //    env.get[Band]("band"),
  //    env.get[SpectroscopyParams]("mode"),
  //    env.get[ItcObservingConditions]("constraints")
  //   ).traverseN { (wv, rs, expTime, exp, sp, sd, mode, c) =>
  //     Logger[F].info(
  //       s"ITC sn calculation for $mode, conditions $c, exposureTime $expTime x $exp and profile $sp"
  //     ) *> {
  //       val signalToNoiseAt = env.get[Wavelength]("signalToNoiseAt")
  //       val specMode        = mode match {
  //         case GmosNSpectroscopyParams(grating, fpu, filter) =>
  //           ObservingMode.SpectroscopyMode.GmosNorth(wv, grating, fpu, filter)
  //         case GmosSSpectroscopyParams(grating, fpu, filter) =>
  //           ObservingMode.SpectroscopyMode.GmosSouth(wv, grating, fpu, filter)
  //       }
  //
  //       graphFromCacheOrRemote(
  //         GraphRequest(TargetProfile(sp, sd, rs), specMode, c, expTime, exp, signalToNoiseAt)
  //       )(itc, redis)
  //         .flatMap { r =>
  //           itc.calculateSignalToNoise(r.charts, signalToNoiseAt)
  //         }
  //     }
  //       .map(_.rightIor[NonEmptyChain[Problem]])
  //       .handleError {
  //         case x: IntegrationTimeError =>
  //           Problem(x.message).leftIorNec
  //         case x                       =>
  //           Problem(s"Error calculating itc $x").leftIorNec
  //       }
  //   }.map(
  //     _.getOrElse(Problem(s"Missing parameters for signal to noise calculation$env").leftIorNec)
  //   )
  //
  def versions[F[_]: Applicative: Logger](
    environment: ExecutionEnvironment,
    redis:       StringCommands[F, Array[Byte], Array[Byte]]
  ): F[Result[ItcVersions]] =
    Result(ItcVersions(version(environment).value, BuildInfo.ocslibHash.some)).pure[F]

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
                RootEffect.computeEncodable("test")((_, p, env) => Result("unsupported").pure[F]),
                RootEffect.computeEncodable("spectroscopyIntegrationTime") { (_, p, env) =>
                  env
                    .getR[SpectroscopyIntegrationTimeInput]("input")
                    .flatMap(toSpectroscopyTimeRequest)
                    .traverse(
                      calculateSpectroscopyIntegrationTime(environment, redis, itc)
                    )
                },
                RootEffect.computeEncodable("imagingIntegrationTime") { (_, p, env) =>
                  env
                    .getR[ImagingIntegrationTimeInput]("input")
                    .flatMap(toImagingTimeRequest)
                    .traverse(
                      calculateImagingIntegrationTime(environment, redis, itc)
                    )
                },
                RootEffect.computeEncodable("optimizedSpectroscopyGraph") { (_, p, env) =>
                  env
                    .getR[OptimizedSpectroscopyGraphInput]("input")
                    .flatMap(toGraphRequest)
                    .traverse(
                      spectroscopyGraph(environment, redis, itc)
                    )
                },
                RootEffect.computeEncodable("spectroscopyIntegrationTimeAndGraph") { (_, p, env) =>
                  println(env
                    .getR[SpectroscopyIntegrationTimeAndGraphInput]("input"))
                  env
                    .getR[SpectroscopyIntegrationTimeAndGraphInput]("input")
                    .flatMap(u => toSpectroscopyTimeRequest(u).map((_, u.significantFigures)))
                    .traverse { case (tr, fig) =>
                      spectroscopyIntegrationTimeAndGraph(environment, redis, itc)(tr, fig)
                    }
                }
              )
            ),
            LeafMapping[BigDecimal](BigDecimalType),
            LeafMapping[Long](LongType),
            LeafMapping[PosInt](PosIntType),
            LeafMapping[SignalToNoise](SignalToNoiseType)
          )

        override val selectElaborator =
          new SelectElaborator(
            Map(
              QueryType -> {
                case Select(
                      "spectroscopyIntegrationTime",
                      List(SpectroscopyIntegrationTimeInput.binding("input", input)),
                      child
                    ) =>
                  input.map(i =>
                    Environment(Cursor.Env(("input", i)), child)
                      .copy(child = Select("spectroscopyIntegrationTime", Nil, child))
                  )

                case Select(
                      "imagingIntegrationTime",
                      List(ImagingIntegrationTimeInput.binding("input", input)),
                      child
                    ) =>
                  input.map(i =>
                    Environment(Cursor.Env(("input", i)), child)
                      .copy(child = Select("imagingIntegrationTime", Nil, child))
                  )

                case Select(
                      "optimizedSpectroscopyGraph",
                      List(OptimizedSpectroscopyGraphInput.binding("input", input)),
                      child
                    ) =>
                  input.map(i =>
                    Environment(Cursor.Env(("input", i)), child)
                      .copy(child = Select("optimizedSpectroscopyGraph", Nil, child))
                  )

                case Select(
                        "spectroscopyIntegrationTimeAndGraph",
                      List(SpectroscopyIntegrationTimeAndGraphInput.binding("input", input)),
                      child
                    ) =>
                  println("-----")
                  println(input)
                  input.map(i =>
                    Environment(Cursor.Env(("input", i)), child)
                      .copy(child = Select("spectroscopyIntegrationTimeAndGraph", Nil, child))
                  )
              }
            )
          )
      }
    }
}
