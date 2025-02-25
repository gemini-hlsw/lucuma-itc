// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import io.circe.literal.*

class GraphQLCustomSedSuite extends GraphQLSuite {

  test("custom sed integration time") {
    query(
      """
        query {
          spectroscopyIntegrationTime(input: {
            exposureTimeMode: {
              signalToNoise: {
                value: 2,
                at: { nanometers: 600 }
              }
            },
            asterism: [
              {
                sourceProfile: {
                  point: {
                    bandNormalized: {
                      sed: {
                        fluxDensities: [
                          { wavelength: { nanometers: 500.0 }, density: 1.0 },
                          { wavelength: { nanometers: 600.0 }, density: 2.0 },
                          { wavelength: { nanometers: 700.0 }, density: 3.0 }
                        ]
                      }
                      brightnesses: [{
                        band: R
                        value: 9
                        units: VEGA_MAGNITUDE
                      }]
                    }
                  }
                },
                radialVelocity: {
                  kilometersPerSecond: 1000
                }
              }
            ],
            constraints: {
              imageQuality: POINT_EIGHT,
              cloudExtinction: ONE_POINT_FIVE,
              skyBackground: BRIGHT,
              waterVapor: MEDIAN,
              elevationRange: {
                airMass: {
                  min: 1,
                  max: 2
                }
              }
            },
            mode: {
              gmosNSpectroscopy: {
                centralWavelength: {
                  nanometers: 60
                },
                filter: GG455,
                fpu: {
                  builtin: LONG_SLIT_0_25
                },
                grating: B1200_G5301
              }
            }
          }) {
            mode {
              ... on SpectroscopyMode {
                instrument
                params {
                  ... on GmosNITCParams {
                    grating
                  }
                }
                centralWavelength {
                  nanometers
                }
              }
            }
            brightest {
              selected {
                exposureCount
                exposureTime {
                  seconds
                }
              }
              band
            }
          }
        }
      """,
      json"""
        {
          "data": {
            "spectroscopyIntegrationTime" : {
              "mode" : {
                "instrument" : "GMOS_NORTH",
                "params": {
                  "grating": "B1200_G5301"
                },
                "centralWavelength" : {
                  "nanometers" : 60.000
                }
              },
              "brightest": {
                "selected" : {
                  "exposureCount" : 10,
                  "exposureTime" : {
                    "seconds" : 1.000000
                  }
                },
                "band": "R"
              }
            }
          }
        }
        """
    )
  }
}
