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

  val queryStrCase1 = """query {
          spectroscopy(input: {
            wavelength: {
              nanometers: 500,
            },
            radialVelocity: {
              centimetersPerSecond: 0
            },
            signalToNoise: 5,
            spatialProfile: {
              sourceType: POINT_SOURCE
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
              imageQuality: POINT_EIGHT,
              cloudExtinction: POINT_THREE,
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
                fpu: LONG_SLIT_1_00,
                disperser: B600_G5307
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

  val body1 = Json.obj("query" -> Json.fromString(queryStrCase1))

  val scn1 = scenario("ITC Case 1")
    .pause(2)
    .exec(
      http("request_1")
        .post("/itc")
        .headers(headers_10)
        .check(status.is(200))
        .check(jsonPath("$.data.spectroscopy[0].results[0].itc.exposures").is("1"))
        .check(jsonPath("$.data.spectroscopy[0].results[0].itc.exposureTime.seconds").is("3"))
        .check(
          jsonPath("$.data.spectroscopy[0].results[0].itc.signalToNoise").is("5.3388133344202195")
        )
        .body(StringBody(body1.noSpaces))
    )

  val queryStrCase2 = """query {
          spectroscopy(input: {
            wavelength: {
              nanometers: 850,
            },
            radialVelocity: {
              centimetersPerSecond: 0
            },
            signalToNoise: 35,
            spatialProfile: {
              sourceType: POINT_SOURCE
            },
            spectralDistribution: {
              stellar: A0V
            },
            magnitude: {
              band: R,
              value: 20,
              error: 1.2,
              system: VEGA
            },
            constraints: {
              imageQuality: POINT_EIGHT,
              cloudExtinction: POINT_THREE,
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
                fpu: LONG_SLIT_0_25,
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

  val body2 = Json.obj("query" -> Json.fromString(queryStrCase2))

  val scn2 = scenario("ITC Case 2")
    .pause(2)
    .exec(
      http("request_2")
        .post("/itc")
        .headers(headers_10)
        .check(status.is(200))
        .check(jsonPath("$.data.spectroscopy[0].results[0].itc.exposures").is("289"))
        .check(jsonPath("$.data.spectroscopy[0].results[0].itc.exposureTime.seconds").is("1199"))
        .check(
          jsonPath("$.data.spectroscopy[0].results[0].itc.signalToNoise").is("35.014480570763865")
        )
        .body(StringBody(body2.noSpaces))
    )

  setUp(
    scn2.inject(atOnceUsers(1)),
    scn1.inject(atOnceUsers(1))
  )
    .assertions(
      forAll.successfulRequests.percent.gt(95.0)
    )
    .protocols(httpProtocol)
}
