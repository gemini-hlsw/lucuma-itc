// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import algebra.instances.all.given
import cats.ApplicativeError
import cats.effect.*
import cats.syntax.all.*
import coulomb.*
import coulomb.ops.algebra.spire.all.given
import coulomb.policy.spire.standard.given
import coulomb.syntax.*
import coulomb.units.si.*
import coulomb.units.si.given
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.numeric.PosLong
import io.circe.Decoder
import io.circe.syntax.*
import lucuma.core.math.Angle
import lucuma.core.model.NonNegDuration
import lucuma.itc.Itc
import lucuma.itc.search.ObservingMode
import lucuma.itc.search.TargetProfile
import lucuma.refined.*
import natchez.Trace
import natchez.http4s.NatchezMiddleware
import org.http4s.*
import org.http4s.circe.*
import org.http4s.client.Client
import org.http4s.client.dsl.Http4sClientDsl
import org.http4s.client.middleware.*
import org.http4s.dsl.io.*
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.syntax.all.*
import org.typelevel.log4cats.Logger

import java.time.Duration
import scala.concurrent.duration.*
import scala.math.*

/** An ITC implementation that calls the OCS2 ITC server remotely. */
object ItcImpl {
  opaque type NumberOfExposures = Int

  def forHeroku[F[_]: Async: Logger: Trace]: Resource[F, Itc[F]] =
    forUri(uri"https://gemini-new-itc.herokuapp.com")

  val Error400Regex = "<title>Error 400 (.*)</title>".r

  def forUri[F[_]: Async: Logger: Trace](uri: Uri): Resource[F, Itc[F]] =
    EmberClientBuilder.default.build
      .map(NatchezMiddleware.client[F])
      .map(RequestLogger(true, false))
      .map(ResponseLogger(true, false))
      .map(forClientAndUri[F](_, uri))

  def forClientAndUri[F[_]: Concurrent: Logger: Trace](c: Client[F], uri: Uri): Itc[F] =
    new Itc[F] with Http4sClientDsl[F] {
      val L = Logger[F]

      def calculate(
        targetProfile: TargetProfile,
        observingMode: ObservingMode,
        constraints:   ItcObservingConditions,
        signalToNoise: BigDecimal
      ): F[Itc.CalcResultWithVersion] =
        observingMode match
          case _: ObservingMode.Spectroscopy =>
            spectroscopy(targetProfile, observingMode, constraints, signalToNoise)
          // TODO: imaging

      def calculateGraph(
        targetProfile: TargetProfile,
        observingMode: ObservingMode,
        constraints:   ItcObservingConditions,
        exposureTime:  NonNegDuration,
        exposures:     PosLong
      ): F[Itc.GraphResult] =
        observingMode match
          case _: ObservingMode.Spectroscopy =>
            spectroscopyGraph(
              targetProfile,
              observingMode,
              constraints,
              BigDecimal(exposureTime.value.toMillis).withUnit[Millisecond].toUnit[Second],
              exposures.value
            )
          // TODO: imaging

      // Convenience method to compute an OCS2 ITC result for the specified profile/mode.
      def itc(
        targetProfile:    TargetProfile,
        observingMode:    ObservingMode,
        constraints:      ItcObservingConditions,
        exposureDuration: Quantity[BigDecimal, Second],
        exposures:        Int,
        level:            NonNegInt
      ): F[legacy.ItcRemoteResult] =
        import lucuma.itc.legacy.given
        import lucuma.itc.legacy.*

        Trace[F].span("legacy-itc-query") {
          val json =
            spectroscopyParams(targetProfile,
                               observingMode,
                               exposureDuration.value.toDouble.seconds,
                               constraints,
                               exposures
            ).asJson
          L.info(s"ITC remote query ${uri / "json"} ${json.noSpaces}") *>
            Trace[F].put("itc.query" -> json.spaces2) *>
            Trace[F].put("itc.exposureDuration" -> exposureDuration.value.toInt) *>
            Trace[F].put("itc.exposures" -> exposures) *>
            Trace[F].put("itc.level" -> level.value) *>
            c.run(POST(json, uri / "json")).use {
              case Status.Successful(resp) =>
                given EntityDecoder[F, ItcRemoteResult] = jsonOf[F, ItcRemoteResult]
                resp.as[ItcRemoteResult]
              case resp                    =>
                resp.bodyText
                  .through(fs2.text.lines)
                  .filter(_.startsWith("<title>"))
                  .compile
                  .last
                  .flatMap {
                    case Some(Error400Regex(msg)) =>
                      L.warn(s"Upstream error $msg") *>
                        ApplicativeError[F, Throwable].raiseError(new UpstreamException(msg))
                    case u                        =>
                      L.warn(s"Upstream error ${u}") *>
                        ApplicativeError[F, Throwable]
                          .raiseError(new UpstreamException(u.getOrElse("Upstream Exception")))
                  }
            }
        }

      override def itcVersions: F[String] =
        import lucuma.itc.legacy.*

        L.info(s"ITC remote query for versions") *>
          c.run(GET(uri / "version")).use {
            case Status.Successful(resp) =>
              given EntityDecoder[F, ItcRemoteVersion] = jsonOf[F, ItcRemoteVersion]
              resp.as[ItcRemoteVersion].map(_.versionToken)
            case resp                    =>
              L.warn(s"Upstream error ${resp}") *>
                ApplicativeError[F, Throwable]
                  .raiseError(new UpstreamException("Upstream Exception"))
          }

      def itcGraph(
        targetProfile:    TargetProfile,
        observingMode:    ObservingMode,
        constraints:      ItcObservingConditions,
        exposureDuration: Quantity[BigDecimal, Second],
        exposures:        Long
      ): F[legacy.ItcRemoteGraphResult] =
        import lucuma.itc.legacy.given
        import lucuma.itc.legacy.*

        Trace[F].span("legacy-itc-query") {
          val json =
            spectroscopyParams(targetProfile,
                               observingMode,
                               exposureDuration.value.toDouble.seconds,
                               constraints,
                               exposures.toInt
            ).asJson
          L.info(s"ITC remote query ${uri / "jsonchart"} ${json.noSpaces}") *>
            Trace[F].put("itc.query" -> json.spaces2) *>
            Trace[F].put("itc.exposureDuration" -> exposureDuration.value.toInt) *>
            Trace[F].put("itc.exposures" -> exposures.toInt) *>
            c.run(POST(json, uri / "jsonchart")).use {
              case Status.Successful(resp) =>
                given EntityDecoder[F, ItcRemoteGraphResult] = jsonOf[F, ItcRemoteGraphResult]
                resp.as[ItcRemoteGraphResult]
              case resp                    =>
                resp.bodyText
                  .through(fs2.text.lines)
                  .filter(_.startsWith("<title>"))
                  .compile
                  .last
                  .flatMap {
                    case Some(Error400Regex(msg)) =>
                      L.warn(s"Upstream error $msg") *>
                        ApplicativeError[F, Throwable].raiseError(new UpstreamException(msg))
                    case u                        =>
                      L.warn(s"Upstream error ${u}") *>
                        ApplicativeError[F, Throwable]
                          .raiseError(new UpstreamException(u.getOrElse("Upstream Exception")))
                  }
            }
        }

      val MaxIterations = 10

      def spectroscopyGraph(
        targetProfile:    TargetProfile,
        observingMode:    ObservingMode,
        constraints:      ItcObservingConditions,
        exposureDuration: Quantity[BigDecimal, Second],
        exposures:        Long
      ): F[Itc.GraphResult] =
        itcGraph(targetProfile, observingMode, constraints, exposureDuration, exposures).map { r =>
          Itc.GraphResult(r.versionToken, r.ccds, r.groups)
        }

      def spectroscopy(
        targetProfile: TargetProfile,
        observingMode: ObservingMode,
        constraints:   ItcObservingConditions,
        signalToNoise: BigDecimal
      ): F[Itc.CalcResultWithVersion] = {
        val startExpTime      = BigDecimal(1200.0).withUnit[Second]
        val numberOfExposures = 1
        val requestedSN       = signalToNoise.toDouble

        // This loops should be necessary only a few times but put a circuit breaker just in case
        def itcStep(
          nExp:       NumberOfExposures,
          oldNExp:    Int,
          expTime:    Quantity[BigDecimal, Second],
          oldExpTime: Quantity[BigDecimal, Second],
          snr:        Double,
          maxTime:    Quantity[BigDecimal, Second],
          s:          legacy.ItcRemoteResult,
          counter:    NonNegInt
        ): F[Itc.CalcResult] =
          if (snr === 0.0) {
            Concurrent[F].raiseError(new ItcCalculationError("S/N obtained is 0"))
          } else {
            val totalTime: Quantity[BigDecimal, Second] =
              expTime * nExp.withUnit[1] * pow(requestedSN / snr, 2).withUnit[1]

            val newNExp: BigDecimal = spire.math.ceil((totalTime / maxTime).value)

            val newExpTime: BigDecimal =
              spire.math.ceil((totalTime / newNExp.withUnit[1]).value)

            val next = NonNegInt.from(counter.value + 1).getOrElse(sys.error("Should not happen"))
            L.info(s"Total time: $totalTime maxTime: $maxTime") *>
              L.info(s"Exp time :$newExpTime s/Num exp $newNExp/iteration $counter") *> {
                if (
                  nExp != oldNExp ||
                  ((expTime - oldExpTime) > 1.withUnit[Second] || (oldExpTime - expTime) > 1
                    .withUnit[Second]) &&
                  counter.value < MaxIterations &&
                  newExpTime < (pow(2, 63) - 1)
                ) {
                  itc(targetProfile,
                      observingMode,
                      constraints,
                      newExpTime.withUnit[Second],
                      newNExp.toInt,
                      next
                  )
                    .flatMap { s =>
                      L.debug(s"-> S/N: ${s.maxTotalSNRatio}") *>
                        itcStep(newNExp.toInt,
                                nExp,
                                newExpTime.withUnit[Second],
                                expTime,
                                s.maxTotalSNRatio,
                                maxTime,
                                s,
                                next
                        )
                    }
                } else
                  Itc.CalcResult
                    .Success(newExpTime.toDouble.seconds, newNExp.toInt, s.maxTotalSNRatio)
                    .pure[F]
                    .widen[Itc.CalcResult]
              }
          }

        L.info(s"Desired S/N $signalToNoise") *>
          L.info(
            s"Target brightness ${targetProfile} at band ${targetProfile.band}"
          ) *>
          Trace[F].span("itc") {

            itc(targetProfile,
                observingMode,
                constraints,
                startExpTime,
                numberOfExposures,
                1.refined
            )
              .flatMap { r =>
                val halfWellTime = r.maxWellDepth / 2 / r.maxPeakPixelFlux * startExpTime.value
                L.info(
                  s"Results CCD wellDepth: ${r.maxWellDepth}, peakPixelFlux: ${r.maxPeakPixelFlux}, totalSNRatio: ${r.maxTotalSNRatio} $halfWellTime"
                ) *> {
                  if (halfWellTime < 1.0) {
                    val msg = s"Target is too bright. Well half filled in $halfWellTime"
                    L.error(msg) *>
                      Itc.CalcResultWithVersion(Itc.CalcResult.SourceTooBright(msg)).pure[F].widen
                  } else {
                    val maxTime = startExpTime.value.min(halfWellTime)
                    itcStep(numberOfExposures,
                            0,
                            startExpTime,
                            BigDecimal(0).withUnit[Second],
                            r.maxTotalSNRatio,
                            maxTime.withUnit[Second],
                            r,
                            0.refined
                    ).map(x => Itc.CalcResultWithVersion(x, r.versionToken.some))
                  }
                }

              }
          }

      }
    }

}
