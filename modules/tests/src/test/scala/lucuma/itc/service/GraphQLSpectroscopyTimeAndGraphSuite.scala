// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import io.circe.literal.*

class GraphQLSpectroscopyTimeAndGraphSuite extends GraphQLSuite {

  test("gmos graph") {
    query(
      """
        query {
          spectroscopyIntegrationTimeAndGraph(input: {
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
                filter: G_PRIME,
                fpu: {
                  builtin: LONG_SLIT_0_25
                },
                grating: B1200_G5301
              }
            },
            significantFigures: {
              xAxis: 4
            }
          }) {
            exposureTime {
              seconds
            }
            exposures
            ccds {
              singleSNRatio
              totalSNRatio
              peakPixelFlux
              wellDepth
              ampGain
            }
            charts {
              chartType
              series {
                title
                seriesType
                data
                xAxis {
                  start
                  end
                  count
                }
                dataY
              }
            }
          }
        }
        """,
      json"""{
          "data": {
            "spectroscopyIntegrationTimeAndGraph": {
              "exposures" : 10,
              "exposureTime" : {
                "seconds" : 1.000000000
              },
              "ccds" : [
                {
                  "singleSNRatio" : 1.0,
                  "totalSNRatio" : 2.0,
                  "peakPixelFlux" : 3.0,
                  "wellDepth" : 4.0,
                  "ampGain" : 5.0
                }
              ],
              "charts": [
                {
                  "chartType": "S2N_CHART",
                  "series": [
                    {
                      "title": "title",
                      "seriesType": "FINAL_S2_NDATA",
                      "data": [
                        [
                          1.0,
                          1000.0
                        ],
                        [
                          2.0,
                          1001.0
                        ]
                      ],
                      "xAxis" : {
                        "start" : 1.0,
                        "end" : 2.0,
                        "count" : 2
                      },
                      "dataY": [
                        1000.0,
                        1001.0
                      ]
                    }
                  ]
                }
              ]
            }
          }
        }
        """
    )
  }
}
