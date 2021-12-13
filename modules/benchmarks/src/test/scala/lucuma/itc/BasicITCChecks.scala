// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import cats.effect.unsafe.implicits.global
import io.circe.Json
import io.gatling.core.Predef._
import io.gatling.http.Predef._
import lucuma.itc.service.Main

import scala.concurrent.Future

class BasicITCChecks extends Simulation {
  var cancelToken: Option[() => Future[Unit]] = None
  before {
    System.setProperty("PORT", "5000")
    System.setProperty("ITC_URL", "https://gemini-new-itc.herokuapp.com/json")
    cancelToken = Some(Main.run(Nil).unsafeRunCancelable())
  }

  after {
    cancelToken.foreach(c => c())
  }
  val headers_10   = Map("Content-Type" -> """application/json""")
  val httpProtocol = http.baseUrl("http://localhost:5000")

  val queryStr = """query {
          spectroscopy(input: {
            wavelength: {
              nanometers: 600,
            },
            radialVelocity: {
              centimetersPerSecond: 0
            },
            signalToNoise: 5,
            spatialProfile: {
              sourceType: UNIFORM_SOURCE
            },
            spectralDistribution: {
              stellar: A0V
            },
            magnitude: {
              band: R,
              value: 15,
              error: 1.2,
              system: VEGA
            },
            constraints: {
              imageQuality: ONE_POINT_ZERO,
              cloudExtinction: ONE_POINT_ZERO,
              skyBackground: GRAY,
              waterVapor: WET,
              elevationRange: {
                airmassRange: {
                  min: 1,
                  max: 1.5
                }
              }
            },
            modes: [{
              gmosN: {
                filter: R_PRIME,
                fpu: LONG_SLIT_1_00,
                disperser: R831_G5302
              }
            }]
          }) {
            results {
                mode {
                  instrument
                  resolution
                  params {
                    ... on GmosNITCParams {
                      disperser
                    }
                  }
                  wavelength {
                    nanometers
                  }
                }
                itc {
                  ... on ItcError {
                    msg
                  }
                  ... on ItcSuccess {
                    exposures
                    exposureTime {
                      seconds
                    }
                    signalToNoise
                  }
                }
            }
          }
        }"""

  val body = Json.obj("query" -> Json.fromString(queryStr))

  val scn = scenario("BasicSimulation")
    .pause(2)
    .exec(
      http("request_1")
        .post("/itc")
        .headers(headers_10)
        .check(status.is(200))
        .check(jsonPath("$.data.spectroscopy[0].results[0].itc.exposures").is("1"))
        .check(jsonPath("$.data.spectroscopy[0].results[0].itc.exposureTime.seconds").is("5"))
        .check(
          jsonPath("$.data.spectroscopy[0].results[0].itc.signalToNoise").is("6.898590514665387")
        )
        .body(StringBody(body.noSpaces))
    )

  setUp(
    scn.inject(atOnceUsers(1))
  )
    .assertions(
      forAll.successfulRequests.percent.gt(95.0)
    )
    .protocols(httpProtocol)
}
