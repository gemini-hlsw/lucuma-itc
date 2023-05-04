// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import cats.syntax.all._
import io.circe.literal._
import lucuma.core.enums._
import lucuma.core.syntax.enumerated._
import lucuma.core.syntax.string._
import lucuma.core.util.Enumerated
import lucuma.itc.ItcObservingConditions

class GraphQLImagingTimeSuite extends GraphQLSuite {

  test("gmos north case") {
    query(
      """
        query {
          imagingIntegrationTime(input: {
            wavelength: {
              picometers: 530000
            },
            signalToNoise: 600,
            sourceProfile: {
              point: {
                bandNormalized: {
                  sed: {
                    stellarLibrary: O5_V
                  },
                  brightnesses: [
                    {
                      band: SLOAN_I,
                      value: 9.484,
                      units: VEGA_MAGNITUDE,
                      error: 0.01
                    },
                    {
                      band: B,
                      value: 8.116,
                      units: VEGA_MAGNITUDE
                    },
                    {
                      band: V,
                      value: 12.323,
                      units: VEGA_MAGNITUDE,
                      error: 0.01
                    },
                    {
                      band: J,
                      value: 14.442,
                      units: VEGA_MAGNITUDE,
                      error: 0.018
                    },
                    {
                      band: H,
                      value: 9.798,
                      units: VEGA_MAGNITUDE,
                      error: 0.029
                    },
                    {
                      band: K,
                      value: 10.65,
                      units: VEGA_MAGNITUDE,
                      error: 0.03
                    }
                  ]
                }
              }
            },
            band: V,
            radialVelocity: {
              metersPerSecond: 7560
            },
            constraints: {
              imageQuality: TWO_POINT_ZERO,
              cloudExtinction: THREE_POINT_ZERO,
              skyBackground: BRIGHT,
              waterVapor: WET,
              elevationRange: {
                airMass: {
                  min: 1,
                  max: 2
                }
              }
            },
            mode: {
              gmosNImaging: {
                filter: G_PRIME
              }
            }
          }) {
            mode {
              __typename
              ... on ImagingMode {
                instrument
                params {
                  ... on GmosNITCParams {
                    filter
                  }
                }
                wavelength {
                  nanometers
                }
              }
            }
            result {
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
          "data": {
            "imagingIntegrationTime" : {
              "mode" : {
                "__typename" : "ImagingMode",
                "instrument" : "GMOS_NORTH",
                "params": {
                  "filter": "G_PRIME"
                },
                "wavelength" : {
                  "nanometers" : 530.000
                }
              },
              "result" : {
                "exposures" : 10,
                "exposureTime" : {
                  "seconds" : 1.000000
                }
              }
            }
          }
        }
        """
    )
  }
}
// wavelength: {
//   nanometers: 60,
// },
// radialVelocity: {
//   kilometersPerSecond: 1000
// },
// signalToNoise: 2,
// sourceProfile: {
//   point: {
//     bandNormalized: {
//       sed: {
//         stellarLibrary: O5_V
//       }
//       brightnesses: [ {
//         band: R
//         value: 3
//         units: ERG_PER_S_PER_CM_SQUARED_PER_A
//       }, {
//         band: J
//         value: 2.1
//         units: AB_MAGNITUDE
//       }]
//     }
//   }
// },
// band: J,
// constraints: {
//   imageQuality: POINT_THREE,
//   cloudExtinction: POINT_FIVE,
//   skyBackground: DARK,
//   waterVapor: DRY,
//   elevationRange: {
//     airMass: {
//       min: 1,
//       max: 2
//     }
//   }
// },
