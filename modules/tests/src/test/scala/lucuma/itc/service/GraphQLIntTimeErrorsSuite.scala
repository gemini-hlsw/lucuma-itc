// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import io.circe.literal.*

class GraphQLIntTimeErrorsSuite extends FailingCalculationSuite {

  test("Test error channel") {
    query(
      """
        query {
          spectroscopyIntegrationTime(input: {
            wavelength: {
              nanometers: 60,
            },
            radialVelocity: {
              kilometersPerSecond: 1000
            },
            signalToNoise: 2,
            sourceProfile: {
              point: {
                bandNormalized: {
                  sed: {
                    stellarLibrary: O5_V
                  }
                  brightnesses: [ {
                    band: R
                    value: 3
                    units: ERG_PER_S_PER_CM_SQUARED_PER_A
                  }, {
                    band: J
                    value: 2.1
                    units: AB_MAGNITUDE
                  }]
                }
              }
            },
            band: J,
            constraints: {
              imageQuality: POINT_THREE,
              cloudExtinction: POINT_FIVE,
              skyBackground: DARK,
              waterVapor: DRY,
              elevationRange: {
                airMass: {
                  min: 1,
                  max: 2
                }
              }
            },
            mode: {
              gmosNSpectroscopy: {
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
                resolution
                params {
                  ... on GmosNITCParams {
                    grating
                  }
                }
                wavelength {
                  nanometers
                }
              }
            }
            results {
              exposures
              exposureTime {
                seconds
              }
            }
          }
        }
        """,
      json"""
        {
          "errors": [{
            "message" : "A calculation error"
          }],
          "data": null
        }
        """
    )
  }
}
