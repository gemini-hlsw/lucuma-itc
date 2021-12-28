// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import cats.Applicative
import cats.effect._
import cats.implicits._
import eu.timepit.refined.auto._
import eu.timepit.refined.types.numeric._
import io.circe.syntax._
import lucuma.core.math.Angle
import lucuma.itc.Itc
import lucuma.itc.search.ObservingMode
import lucuma.itc.search.TargetProfile
import natchez.Trace
import natchez.http4s.NatchezMiddleware
import org.http4s._
import org.http4s.asynchttpclient.client.AsyncHttpClient
import org.http4s.circe._
import org.http4s.client.Client
import org.http4s.client.dsl.Http4sClientDsl
import org.http4s.client.middleware._
import org.http4s.dsl.io._
import org.http4s.syntax.all._
import org.typelevel.log4cats.Logger

import scala.concurrent.duration._
import scala.math._

/** An ITC implementation that calls the OCS2 ITC server remotely. */
object ItcImpl {

  def forHeroku[F[_]: Async: Logger: Trace]: Resource[F, Itc[F]] =
    forUri(uri"https://gemini-itc.herokuapp.com/json")

  def forUri[F[_]: Async: Logger: Trace](uri: Uri): Resource[F, Itc[F]] =
    AsyncHttpClient
      .resource(AsyncHttpClient.configure(_.setRequestTimeout(30000)))
      .map(NatchezMiddleware.client[F])
      .map(RequestLogger(true, true))
      .map(ResponseLogger(true, true))
      .map(forClientAndUri[F](_, uri))

  def forClientAndUri[F[_]: Concurrent: Logger: Trace](c: Client[F], uri: Uri): Itc[F] =
    new Itc[F] with Http4sClientDsl[F] {
      val L = Logger[F]

      def calculate(
        targetProfile: TargetProfile,
        observingMode: ObservingMode,
        constraints:   ItcObservingConditions,
        signalToNoise: BigDecimal
      ): F[Itc.Result] =
        observingMode match {
          case _: ObservingMode.Spectroscopy =>
            spectroscopy(targetProfile, observingMode, constraints, signalToNoise)
          // TODO: imaging
        }

      // Convenience method to compute an OCS2 ITC result for the specified profile/mode.
      def itc(
        targetProfile:    TargetProfile,
        observingMode:    ObservingMode,
        constraints:      ItcObservingConditions,
        exposureDuration: FiniteDuration,
        exposures:        Int,
        level:            NonNegInt
      ): F[ItcResult] =
        Trace[F].span("legacy-itc-query") {
          val json =
            spectroscopyParams(targetProfile,
                               observingMode,
                               exposureDuration,
                               constraints,
                               exposures
            ).asJson
          L.info(s"ITC remote query ${json.noSpaces}") *>
            Trace[F].put("itc.query" -> json.spaces2) *>
            Trace[F].put("itc.exposureDuration" -> exposureDuration.toMillis.toInt) *>
            Trace[F].put("itc.exposures" -> exposures) *>
            Trace[F].put("itc.level" -> level.value) *>
            c.expect(POST(json, uri))(jsonOf[F, ItcResult]).attemptTap {
              case Left(e) => L.warn(e)("Remote ITC error")
              case _       => Applicative[F].unit
            }

        }

      val MaxIterations = 10

      def spectroscopy(
        targetProfile: TargetProfile,
        observingMode: ObservingMode,
        constraints:   ItcObservingConditions,
        signalToNoise: BigDecimal
      ): F[Itc.Result] = {
        val startExpTime      = 1200.0
        val numberOfExposures = 1
        val requestedSN       = signalToNoise.toDouble

        // This loops should be necessary only a few times but put a circuit breaker just in case
        def itcStep(
          nExp:       Int,
          oldNExp:    Int,
          expTime:    Double,
          oldExpTime: Double,
          snr:        Double,
          maxTime:    Double,
          s:          ItcResult,
          counter:    NonNegInt
        ): F[Itc.Result] = {
          val totalTime  = expTime * nExp.toDouble * pow(requestedSN / snr, 2)
          val newNExp    = ceil(totalTime / maxTime)
          val newExpTime = ceil(totalTime / newNExp)
          val next       = NonNegInt.from(counter.value + 1).getOrElse(sys.error("Should not happen"))
          L.debug(s"Total time: $totalTime") *>
            L.debug(s"Exp time :$newExpTime s/Num exp $newNExp/iteration $counter") *> {
              if (nExp != oldNExp || abs(expTime - oldExpTime) > 1 && counter < MaxIterations) {
                itc(targetProfile,
                    observingMode,
                    constraints,
                    newExpTime.seconds,
                    newNExp.toInt,
                    next
                )
                  .flatMap { s =>
                    L.debug(s"-> S/N: ${s.maxTotalSNRatio}") *>
                      itcStep(newNExp.toInt,
                              nExp,
                              newExpTime,
                              expTime,
                              s.maxTotalSNRatio,
                              maxTime,
                              s,
                              next
                      )
                  }
              } else
                Itc.Result
                  .Success(newExpTime.seconds, newNExp.toInt, s.maxTotalSNRatio)
                  .pure[F]
                  .widen[Itc.Result]
            }

        }

        L.info(s"Desired S/N $signalToNoise") *>
          L.info(
            s"Target brightness ${targetProfile.magnitude.value.toDoubleValue} at band ${targetProfile.magnitude.band}"
          ) *>
          Trace[F].span("itc") {

            itc(targetProfile,
                observingMode,
                constraints,
                startExpTime.seconds,
                numberOfExposures,
                1
            )
              .flatMap { r =>
                val halfWellTime = r.maxWellDepth / 2 / r.maxPeakPixelFlux * startExpTime
                L.info(
                  s"Results CCD wellDepth: ${r.maxWellDepth}, peakPixelFlux: ${r.maxPeakPixelFlux}, totalSNRatio: ${r.maxTotalSNRatio}"
                ) *> {
                  if (halfWellTime < 1.0) {
                    val msg = s"Target is too bright. Well half filled in $halfWellTime"
                    L.error(msg) *> Itc.Result.SourceTooBright(msg).pure[F].widen[Itc.Result]
                  } else {
                    val maxTime = min(startExpTime, halfWellTime)

                    itcStep(numberOfExposures, 0, startExpTime, 0, maxTime, r.maxTotalSNRatio, r, 0)
                  }
                }

              }
          }

      }
    }

  // def oldSpectroscopy[F[_]: Trace: Applicative](
  //   targetProfile: TargetProfile,
  //   observingMode: ObservingMode,
  //   constraints:   ItcObservingConditions,
  //   signalToNoise: BigDecimal
  // ): F[Itc.Result] = {
  //
  //  val MaxPercentSaturation = 50.0
  //
  //   // The OCS2 ITC doesn't know how to compute exposure time and exposure count for a
  //   // requested signal-to-noise so we have to kind of wank around to estimate it here, which
  //   // can require up to three round-trip calls to ITC. We can push some logic back over there
  //   // at some point but this is ok for now.
  //
  //   // Pull our exposure limits out of the observing mode since we'll need them soon.
  //   // N.B. we can't just import these because they're added to `Instrument` with syntax.
  //   val minExposureDuration: FiniteDuration = observingMode.instrument.minExposureDuration
  //   val maxExposureDuration: FiniteDuration = observingMode.instrument.maxExposureDuration
  //   val integralDurations: Boolean          = observingMode.instrument.integralDurations
  //
  //   // Compute a 1-second exposure and use this as a baseline for estimating longer/multiple
  //   // exposures. All of our estimations are just that: we can't deal with read noise accurately
  //   // here so the final 9kresult will be approximately the S/N the user requests. We just have to
  //   // accept this limitation for now. Note that conditions are totally guesswork so this may
  //   // end up being good enough. Unclear.
  //
  //   Trace[F].span("itc") {
  //     itc(targetProfile, observingMode, constraints, 1.second, 1, 1).flatMap { baseline =>
  //       // First thing we need to check is the saturation for a minimum-length exposure. If it's
  //       // greater than our limit we simply can't observe in this mode because the source is
  //       // too bright.
  //
  //       if (
  //         baseline.maxPercentFullWell * minExposureDuration.toDoubleSeconds > MaxPercentSaturation
  //       ) {
  //
  //         (Itc.Result.SourceTooBright("too bright"): Itc.Result).pure[F]
  //
  //       } else {
  //
  //         // Ok so we know that it's possible to observe this thing. Let's scale to get an ideal
  //         // single exposure time. If it's within instrument limits and doesn't saturate
  //         // the detector then we can do the whole thing in a single exposure.
  //
  //         val singleExposureDuration: FiniteDuration =
  //           (signalToNoise * signalToNoise / (baseline.maxTotalSNRatio * baseline.maxTotalSNRatio)).toInt.seconds
  //             .secondsCeilIf(integralDurations)
  //
  //         val singleExposureSaturation: Double =
  //           baseline.maxPercentFullWell * singleExposureDuration.toDoubleSeconds
  //
  //         if (
  //           singleExposureDuration >= minExposureDuration &&
  //           singleExposureDuration <= maxExposureDuration &&
  //           singleExposureSaturation <= MaxPercentSaturation
  //         ) {
  //
  //           // We can do this in one exposure, but we need to hit ITC again to get an accurate
  //           // signal-to-noise value.
  //           itc(targetProfile, observingMode, constraints, singleExposureDuration, 1, 2).map { r =>
  //             Itc.Result.Success(singleExposureDuration, 1, r.maxTotalSNRatio.toInt)
  //           }
  //
  //         } else {
  //
  //           // For multiple exposures we compute the time it would take to fill the well to 50%,
  //           // then clip to instrument limits and round up to the nearest second if necessary.
  //
  //           val multipleExposureSecs: FiniteDuration =
  //             (MaxPercentSaturation / baseline.maxPercentFullWell).seconds
  //               .min(maxExposureDuration)
  //               .max(minExposureDuration)
  //               .secondsCeilIf(integralDurations)
  //
  //           // We can't compute S/N accurately enough to extrapolate the number of exposures we
  //           // need so we must ask ITC to do it.
  //           itc(targetProfile, observingMode, constraints, multipleExposureSecs, 1, 2).flatMap {
  //             r =>
  //               // Now estimate the number of exposures. It may be low due to read noise but we
  //               // don't really have a way to compensate yet.
  //               val n =
  //                 ((signalToNoise * signalToNoise) / (r.maxTotalSNRatio * r.maxTotalSNRatio)).toFloat.ceil.toInt
  //
  //               // But in any case we can calculate our final answer, which may come in low.
  //               itc(targetProfile, observingMode, constraints, multipleExposureSecs, n, 3).map {
  //                 r2 =>
  //                   Itc.Result.Success(multipleExposureSecs, n, r2.maxTotalSNRatio.toInt)
  //               }
  //
  //           }
  //
  //         }
  //
  //       }
  //
  //     }
  // }

  // }

  /** Convert model types into OCS2 ITC-compatible types for a spectroscopy request. */
  private def spectroscopyParams(
    targetProfile:    TargetProfile,
    observingMode:    ObservingMode,
    exposureDuration: FiniteDuration,
    conditions:       ItcObservingConditions,
    exposures:        Int
  ): ItcParameters =
    ItcParameters(
      source = ItcSourceDefinition.fromTargetProfile(targetProfile),
      observation = ItcObservationDetails(
        calculationMethod = ItcObservationDetails.CalculationMethod.SignalToNoise.Spectroscopy(
          exposures = exposures,
          coadds = None,
          exposureDuration = exposureDuration,
          sourceFraction = 1.0,
          ditherOffset = Angle.Angle0
        ),
        analysisMethod = observingMode.analysisMethod
      ),
      conditions = conditions,
      telescope = ItcTelescopeDetails(
        wfs = ItcWavefrontSensor.OIWFS
      ),
      instrument = ItcInstrumentDetails.fromObservingMode(observingMode)
    )

}
