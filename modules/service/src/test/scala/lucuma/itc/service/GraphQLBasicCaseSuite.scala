// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import cats.effect._
import io.circe.literal._
import org.http4s._
import org.http4s.syntax.all._

class GraphQLBasicCaseSuite extends GraphQLSuite {

  test("empty post") {
    IO(itcFixture())
      .flatMap { itc =>
        itc.orNotFound.run(Request(method = Method.POST, uri = uri"/itc"))
      }
      .intercept[MalformedMessageBodyFailure]
  }

  test("multiple wv units") {
    query(
      """
        query {
          basiccase(input: {
            wavelength: {
              nanometers: 60,
              picometers: 300
            },
            radialVelocity: {
              kilometersPerSecond: 1000
            },
            simultaneousCoverage: {
              picometers: 200
            },
            resolution: 10,
            signalToNoise: 2,
            spatialProfile: {
              sourceType: POINT_SOURCE
            },
            spectralDistribution: {
              nonStellar: QS02
            },
            magnitude: {
              band: Y,
              system: AB,
              value: 5
            }
          }) {
            results {
              itc {
                ... on ItcSuccess {
                  exposureTime {
                    milliseconds
                  }
                }
              }
            }
          }
        }
        """,
      json"""{
        "errors": [
          {"message": "Wavelength defined with multiple units {picometers, nanometers}"}
        ]
      }"""
    )
  }

  test("bad radial velocity") {
    query(
      """
        query {
          basiccase(input: {
            wavelength: {
              picometers: 300
            },
            radialVelocity: {
              kilometersPerSecond: ddd
            },
            simultaneousCoverage: {
              picometers: 200
            },
            resolution: 10,
            signalToNoise: 2,
            spatialProfile: {
              sourceType: POINT_SOURCE
            },
            spectralDistribution: {
              stellar: A0III
            },
            magnitude: {
              band: UC,
              system: AB,
              value: 5
            }
          })
          {
            results {
              itc {
                ... on ItcSuccess {
                  exposureTime {
                    seconds
                  }
                }
              }
            }
          }
        }
        """,
      json"""{
        "errors": [
          {"message" : "Radial Velocity value is not valid List((centimetersPerSecond,AbsentValue), (metersPerSecond,AbsentValue), (kilometersPerSecond,UntypedEnumValue(ddd)))"}
        ]
      }"""
    )
  }

  test("bad radial velocity and wavelength") {
    query(
      """
        query {
          basiccase(input: {
            wavelength: {
              picometers: 300,
              nanometers: 200
            },
            radialVelocity: {
              kilometersPerSecond: ddd
            },
            simultaneousCoverage: {
              picometers: 200
            },
            resolution: 10,
            signalToNoise: 2,
            spatialProfile: {
              sourceType: POINT_SOURCE
            },
            spectralDistribution: {
              stellar: A0III
            },
            magnitude: {
              band: Y,
              system: AB,
              value: 5
            }
          }) {
            results {
              itc {
                ... on ItcSuccess {
                  exposureTime {
                    microseconds
                  }
                }
              }
            }
          }
        }
        """,
      json"""{
        "errors": [
          {"message": "Wavelength defined with multiple units {picometers, nanometers}"},
          {"message" : "Radial Velocity value is not valid List((centimetersPerSecond,AbsentValue), (metersPerSecond,AbsentValue), (kilometersPerSecond,UntypedEnumValue(ddd)))"}
        ]
      }"""
    )
  }

  test("default case") {
    query(
      """
        query {
          basiccase(input: {
            wavelength: {
              nanometers: 60,
            },
            radialVelocity: {
              kilometersPerSecond: 100
            },
            simultaneousCoverage: {
              nanometers: 200
            },
            resolution: 10,
            signalToNoise: 2,
            spatialProfile: {
              sourceType: POINT_SOURCE
            },
            spectralDistribution: {
              blackBody: {
                temperature: 50.1
              }
            },
            magnitude: {
              band: AP,
              value: 5,
              error: 1.2,
              system: JY
            }
          }) {
            results {
              mode {
                wavelength {
                  nanometers
                }
              }
              itc {
                ... on ItcSuccess {
                  exposures
                  exposureTime {
                    seconds
                  }
                }
              }
            }
          }
        }
        """,
      json"""
        {
          "data": {
            "basiccase": {
              "results": [
                {
                  "mode": {
                    "wavelength": {
                      "nanometers": 1.00
                    }
                  },
                  "itc": {
                    "exposureTime": {
                      "seconds": 1
                    },
                    "exposures": 10
                  }
                }
              ]
            }
          }
        }
      """
    )
  }

  test("multiple spectral distribution") {
    query(
      """
        query {
          basiccase(input: {
            wavelength: {
              nanometers: 60,
            },
            simultaneousCoverage: {
              nanometers: 200
            },
            resolution: 10,
            radialVelocity: {
              kilometersPerSecond: 5
            },
            signalToNoise: 2,
            spatialProfile: {
              sourceType: POINT_SOURCE
            },
            spectralDistribution: {
              blackBody: {
                temperature: 50.1
              },
              powerLaw: {
                index: 100
              }
            },
            magnitude: {
              band: Y,
              system: AB,
              value: 5
            }
          }) {
            results {
              mode {
                wavelength {
                  nanometers
                }
              }
              itc {
                ... on ItcSuccess {
                  exposures
                }
              }
            }
          }
        }
        """,
      json"""
      {
        "errors": [
          {"message": "Spectral distribution value is not valid {blackBody, powerLaw}"}
        ]
      }
      """
    )
  }

  test("gaussian source") {
    query(
      """
        query {
          basiccase(input: {
            wavelength: {
              nanometers: 60,
            },
            radialVelocity: {
              kilometersPerSecond: 10
            },
            simultaneousCoverage: {
              nanometers: 200
            },
            resolution: 10,
            signalToNoise: 2,
            spatialProfile: {
              sourceType: GAUSSIAN_SOURCE,
              fwhm: {
                microarcseconds: 1000
              }
            },
            spectralDistribution: {
              powerLaw: {
                index: 1000
              }
            },
            magnitude: {
              band: Y,
              system: AB,
              value: 5
            }
          }) {
            results {
              mode {
                wavelength {
                  picometers
                }
              }
              itc {
                ... on ItcSuccess {
                  exposures
                }
              }
            }
          }
        }
        """,
      json"""{
        "data": {
          "basiccase": {
            "results": [
              {
                "mode": {
                  "wavelength": {
                    "picometers": 1000
                  }
                },
                "itc": {
                    "exposures": 10
                }
              }
            ]
          }
        }
      }"""
    )
  }

}
