// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import buildinfo.BuildInfo
import cats.*
import cats.data.NonEmptyChain
import cats.effect.*
import cats.syntax.all.*
import dev.profunktor.redis4cats.algebra.StringCommands
import eu.timepit.refined.*
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.types.numeric.PosInt
import grackle.*
import grackle.circe.CirceMapping
import io.circe.syntax.*
import lucuma.core.data.Zipper
import lucuma.core.enums.{ExecutionEnvironment as _, *}
import lucuma.core.math.SignalToNoise
import lucuma.core.util.TimeSpan
import lucuma.itc.*
import lucuma.itc.ItcVersions
import lucuma.itc.encoders.given
import lucuma.itc.input.*
import lucuma.itc.search.ObservingMode
import lucuma.itc.service.config.*
import lucuma.itc.service.requests.*
import lucuma.itc.service.syntax.all.*
import natchez.Trace
import org.typelevel.log4cats.Logger

import scala.io.Source
import scala.util.Using

import Query.*
import QueryCompiler.*

object ItcMapping extends ItcCacheOrRemote with Version {

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

  def versions[F[_]: Applicative: Logger](
    environment: ExecutionEnvironment,
    redis:       StringCommands[F, Array[Byte], Array[Byte]]
  ): F[Result[ItcVersions]] =
    Result(ItcVersions(version(environment).value, BuildInfo.ocslibHash.some)).pure[F]

  private val buildError: Throwable => Error =
    case SourceTooBright(hw)       => Error.SourceTooBright(hw)
    case UpstreamException(m)      => Error.General(m.mkString("\n"))
    case IntegrationTimeError(msg) => Error.General(msg)
    case t                         => Error.General(s"Error calculating ITC: ${t.getMessage}")

  private def buildTargetIntegrationTime(
    ccdTimes: NonEmptyChain[IntegrationTime],
    band:     Band
  ): TargetIntegrationTime =
    TargetIntegrationTime(Zipper.of(ccdTimes.head, ccdTimes.tail.toList*), band)

  private def errorToProblem(error: Error, targetIndex: Int): Problem =
    Problem(error.message, extensions = ErrorExtension(targetIndex, error).asJsonObject.some)

  extension (timeResult: IntegrationTimeCalculationResult)
    private def toResult: Result[IntegrationTimeCalculationResult] =
      timeResult.targetTimes.collectErrors.fold(Result.success(timeResult)): errors =>
        Result.Warning(errors.map(errorToProblem), timeResult)

  extension (graphResult: SpectroscopyGraphsResult)
    private def toResult: Result[SpectroscopyGraphsResult] =
      graphResult.targetGraphs.collectErrors.fold(Result.success(graphResult)): errors =>
        Result.Warning(errors.map(errorToProblem), graphResult)

  def calculateSpectroscopyIntegrationTime[F[_]: MonadThrow: Logger: Parallel: Trace: Clock](
    environment:     ExecutionEnvironment,
    redis:           StringCommands[F, Array[Byte], Array[Byte]],
    itc:             Itc[F]
  )(
    asterismRequest: AsterismSpectroscopyTimeRequest
  ): F[Result[IntegrationTimeCalculationResult]] =
    asterismRequest.toTargetRequests
      .parTraverse: (targetRequest: TargetSpectroscopyTimeRequest) =>
        specTimeFromCacheOrRemote(targetRequest)(itc, redis).attempt
          .map: (result: Either[Throwable, (NonEmptyChain[IntegrationTime], Band)]) =>
            TargetIntegrationTimeOutcome:
              result.bimap(buildError, buildTargetIntegrationTime)
      .map: (targetOutcomes: NonEmptyChain[TargetIntegrationTimeOutcome]) =>
        IntegrationTimeCalculationResult(
          ItcVersions(version(environment).value, BuildInfo.ocslibHash.some),
          asterismRequest.specMode,
          AsterismIntegrationTimeOutcomes(targetOutcomes)
        ).toResult
      .onError: t =>
        Logger[F]
          .error(t)(
            s"Error calculating spectroscopy integration time for input: $asterismRequest"
          )

  def calculateImagingIntegrationTime[F[_]: MonadThrow: Logger: Parallel: Trace: Clock](
    environment: ExecutionEnvironment,
    redis:       StringCommands[F, Array[Byte], Array[Byte]],
    itc:         Itc[F]
  )(asterismRequest: AsterismImagingTimeRequest): F[Result[IntegrationTimeCalculationResult]] =
    asterismRequest.toTargetRequests
      .parTraverse: (targetRequest: TargetImagingTimeRequest) =>
        imgTimeFromCacheOrRemote(targetRequest)(itc, redis).attempt
          .map: (result: Either[Throwable, (NonEmptyChain[IntegrationTime], Band)]) =>
            TargetIntegrationTimeOutcome:
              result
                .bimap(buildError, buildTargetIntegrationTime)
                .map: (integrationTime: TargetIntegrationTime) =>
                  asterismRequest.imagingMode match
                    case ObservingMode.ImagingMode.GmosNorth(_, _) |
                        ObservingMode.ImagingMode.GmosSouth(_, _) =>
                      integrationTime
                        .focusIndex(1) // For gmos focus on the second CCD
                        .getOrElse(integrationTime)
      .map: (targetOutcomes: NonEmptyChain[TargetIntegrationTimeOutcome]) =>
        IntegrationTimeCalculationResult(
          ItcVersions(version(environment).value, BuildInfo.ocslibHash.some),
          asterismRequest.imagingMode,
          AsterismIntegrationTimeOutcomes(targetOutcomes)
        ).toResult
      .onError: t =>
        Logger[F]
          .error(t)(s"Error calculating imaging integration time for input: $asterismRequest")

  private def buildTargetGraphsResult(significantFigures: Option[SignificantFigures])(
    graphResult: TargetGraphsCalcResult,
    band:        Band
  ): TargetGraphsResult = {
    val graphs: NonEmptyChain[ItcGraphGroup] =
      significantFigures.fold(graphResult.data): v =>
        graphResult.data.map(_.adjustSignificantFigures(v))

    val ccds: NonEmptyChain[ItcCcd] =
      significantFigures.fold(graphResult.ccds): v =>
        graphResult.ccds.map(_.adjustSignificantFigures(v))

    val peakFinalSNRatio =
      significantFigures.fold(graphResult.peakFinalSNRatio):
        graphResult.peakFinalSNRatio.adjustSignificantFigures

    val peakSingleSNRatio =
      significantFigures.fold(graphResult.peakSingleSNRatio):
        graphResult.peakSingleSNRatio.adjustSignificantFigures

    val atWvFinalSNRatio: Option[FinalSN] =
      significantFigures.fold(graphResult.atWavelengthFinalSNRatio): s =>
        graphResult.atWavelengthFinalSNRatio.map(_.adjustSignificantFigures(s))

    val atWvSingleSNRatio: Option[SingleSN] =
      significantFigures.fold(graphResult.atWavelengthSingleSNRatio): s =>
        graphResult.atWavelengthSingleSNRatio.map(_.adjustSignificantFigures(s))

    TargetGraphsResult(
      TargetGraphs(
        ccds,
        graphs.flatMap(_.graphs),
        peakFinalSNRatio,
        atWvFinalSNRatio,
        peakSingleSNRatio,
        atWvSingleSNRatio
      ),
      band
    )
  }

  def spectroscopyGraphs[F[_]: MonadThrow: Logger: Parallel: Trace: Clock](
    environment: ExecutionEnvironment,
    redis:       StringCommands[F, Array[Byte], Array[Byte]],
    itc:         Itc[F]
  )(asterismRequest: AsterismGraphRequest): F[Result[SpectroscopyGraphsResult]] =
    asterismRequest.toTargetRequests
      .parTraverse: (targetRequest: TargetGraphRequest) =>
        graphFromCacheOrRemote(targetRequest)(itc, redis).attempt
          .map: (result: Either[Throwable, (TargetGraphsCalcResult, Band)]) =>
            TargetGraphsOutcome:
              result.bimap(buildError, buildTargetGraphsResult(asterismRequest.significantFigures))
      .map: (targetGraphs: NonEmptyChain[TargetGraphsOutcome]) =>
        SpectroscopyGraphsResult(
          ItcVersions(version(environment).value, BuildInfo.ocslibHash.some),
          AsterismTargetGraphsOutcomes(targetGraphs)
        ).toResult
      .onError: t =>
        Logger[F]
          .error(t)(s"Error calculating spectroscopy graph for input: $asterismRequest")

  private def buildAsterismGraphRequest(
    asterismRequest: AsterismSpectroscopyTimeRequest,
    figures:         Option[SignificantFigures]
  )(integrationTimes: AsterismIntegrationTimes): AsterismGraphRequest = {
    val brightestTarget: TargetIntegrationTime = integrationTimes.value.focus
    val selectedVariation: IntegrationTime     = brightestTarget.times.focus
    val expTime: TimeSpan                      = selectedVariation.exposureTime
    val expCount: PosInt                       = selectedVariation.exposureCount

    AsterismGraphRequest(
      asterismRequest.asterism,
      GraphParameters(
        asterismRequest.atWavelength,
        asterismRequest.specMode,
        asterismRequest.constraints,
        expTime,
        expCount
      ),
      figures
    )
  }

  def spectroscopyIntegrationTimeAndGraphs[F[_]: MonadThrow: Logger: Parallel: Trace: Clock](
    environment:     ExecutionEnvironment,
    redis:           StringCommands[F, Array[Byte], Array[Byte]],
    itc:             Itc[F]
  )(
    asterismRequest: AsterismSpectroscopyTimeRequest,
    figures:         Option[SignificantFigures]
  ): F[Result[SpectroscopyTimeAndGraphsResult]] =
    ResultT(calculateSpectroscopyIntegrationTime(environment, redis, itc)(asterismRequest))
      .flatMap: (specTimeResults: IntegrationTimeCalculationResult) =>
        specTimeResults.targetTimes.partitionErrors
          .bimap(
            // If there was an error computing integration times, we cannot compute the graphs
            // and we short circuit to just returning the times we could get and errors.
            _ => specTimeResults.targetTimes.pure[ResultT[F, *]],
            // If integration times were all successful, compute graphs with brightest target's times.
            (integrationTimes: AsterismIntegrationTimes) =>
              val graphRequest: AsterismGraphRequest =
                buildAsterismGraphRequest(asterismRequest, figures)(integrationTimes)
              ResultT(spectroscopyGraphs(environment, redis, itc)(graphRequest)).map:
                (graphResult: SpectroscopyGraphsResult) =>
                  AsterismTimeAndGraphs.fromTimeAndGraphResults(integrationTimes, graphResult)
          )
          .bisequence
          .map: (timesOrGraphs: Either[AsterismIntegrationTimeOutcomes, AsterismTimeAndGraphs]) =>
            SpectroscopyTimeAndGraphsResult(
              specTimeResults.versions,
              AsterismTimesAndGraphsOutcomes(timesOrGraphs)
            )
      .value
      .onError: t =>
        Logger[F]
          .error(t):
            s"Error calculating spectroscopy integration time and graph for input: $asterismRequest"

  def apply[F[_]: Sync: Logger: Parallel: Trace](
    environment: ExecutionEnvironment,
    redis:       StringCommands[F, Array[Byte], Array[Byte]],
    itc:         Itc[F]
  ): F[Mapping[F]] =
    loadSchema[F].map { loadedSchema =>
      new CirceMapping[F] {

        override def parserConfig: GraphQLParser.Config = // set a more reasonable input depth limit
          GraphQLParser.defaultConfig.copy(maxInputValueDepth = 16)

        val schema: Schema    = loadedSchema
        val QueryType         = schema.ref("Query")
        val BigDecimalType    = schema.ref("BigDecimal")
        val LongType          = schema.ref("Long")
        val PosIntType        = schema.ref("PosInt")
        val SignalToNoiseType = schema.ref("SignalToNoise")

        val typeMappings: TypeMappings =
          List(
            ObjectMapping(
              tpe = QueryType,
              fieldMappings = List(
                RootEffect.computeEncodable("versions")((p, env) => versions(environment, redis)),
                RootEffect.computeEncodable("spectroscopyIntegrationTime") { (p, env) =>
                  env
                    .getR[SpectroscopyIntegrationTimeInput]("input")
                    .flatMap(AsterismSpectroscopyTimeRequest.fromInput)
                    .flatTraverse:
                      calculateSpectroscopyIntegrationTime(environment, redis, itc)
                },
                RootEffect.computeEncodable("imagingIntegrationTime") { (p, env) =>
                  env
                    .getR[ImagingIntegrationTimeInput]("input")
                    .flatMap(AsterismImagingTimeRequest.fromInput)
                    .flatTraverse:
                      calculateImagingIntegrationTime(environment, redis, itc)
                },
                RootEffect.computeEncodable("spectroscopyGraphs") { (p, env) =>
                  env
                    .getR[SpectroscopyGraphsInput]("input")
                    .flatMap(AsterismGraphRequest.fromInput)
                    .flatTraverse:
                      spectroscopyGraphs(environment, redis, itc)
                },
                RootEffect.computeEncodable("spectroscopyIntegrationTimeAndGraphs") { (p, env) =>
                  env
                    .getR[SpectroscopyIntegrationTimeAndGraphsInput]("input")
                    .flatMap: input =>
                      AsterismSpectroscopyTimeRequest
                        .fromInput(input)
                        .map((_, input.significantFigures))
                    .flatTraverse: (tr, fig) =>
                      spectroscopyIntegrationTimeAndGraphs(environment, redis, itc)(tr, fig)
                }
              )
            ),
            LeafMapping[BigDecimal](BigDecimalType),
            LeafMapping[Long](LongType),
            LeafMapping[PosInt](PosIntType),
            LeafMapping[SignalToNoise](SignalToNoiseType)
          )

        override val selectElaborator =
          def handle[A](input: Result[A]): Elab[Unit] =
            Elab.liftR(input).flatMap(i => Elab.env("input" -> i))
          SelectElaborator {
            case (QueryType,
                  "spectroscopyIntegrationTime",
                  List(SpectroscopyIntegrationTimeInput.binding("input", input))
                ) =>
              handle(input)
            case (QueryType,
                  "imagingIntegrationTime",
                  List(ImagingIntegrationTimeInput.binding("input", input))
                ) =>
              handle(input)
            case (QueryType,
                  "spectroscopyGraphs",
                  List(SpectroscopyGraphsInput.binding("input", input))
                ) =>
              handle(input)
            case (QueryType,
                  "spectroscopyIntegrationTimeAndGraphs",
                  List(SpectroscopyIntegrationTimeAndGraphsInput.binding("input", input))
                ) =>
              handle(input)
          }

      }
    }
}
