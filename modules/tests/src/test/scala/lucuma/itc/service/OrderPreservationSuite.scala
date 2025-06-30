// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import io.circe.literal.*

class OrderPreservationSuite extends GraphQLSuite:

  test("spectroscopy GMOS modes preserve input order"):
    query(
      """
        query {
          spectroscopy(input: {
            exposureTimeMode: {
              timeAndCount: {
                time: {
                  seconds: 2
                },
                count: 3,
                at: { nanometers: 600 }
              }
            },
            asterism: [
              {
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
            modes: [
              {
                gmosNSpectroscopy: {
                  centralWavelength: {
                    nanometers: 600
                  },
                  filter: GG455,
                  fpu: {
                    builtin: LONG_SLIT_0_25
                  },
                  grating: B1200_G5301
                }
              },
              {
                gmosNSpectroscopy: {
                  centralWavelength: {
                    nanometers: 650
                  },
                  filter: GG455,
                  fpu: {
                    builtin: LONG_SLIT_0_25
                  },
                  grating: R831_G5302
                }
              }
            ]
          }) {
            all {
              mode {
                instrument
                ... on SpectroscopyMode {
                  params {
                    ... on GmosNSpectroscopyParams {
                      grating
                      filter
                      centralWavelength {
                        nanometers
                      }
                    }
                  }
                }
              }
              targetTimes {
                ... on TargetIntegrationTime {
                  selected {
                    exposureCount
                    exposureTime {
                      seconds
                    }
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
            "spectroscopy": {
              "all": [
                {
                  "mode": {
                    "instrument": "GMOS_NORTH",
                    "params": {
                      "grating": "B1200_G5301",
                      "filter": "GG455",
                      "centralWavelength": {
                        "nanometers": 600.000
                      }
                    }
                  },
                  "targetTimes": [
                    {
                      "selected": {
                        "exposureCount": 10,
                        "exposureTime": {
                          "seconds": 2.000000
                        }
                      }
                    }
                  ]
                },
                {
                  "mode": {
                    "instrument": "GMOS_NORTH",
                    "params": {
                      "grating": "R831_G5302",
                      "filter": "GG455",
                      "centralWavelength": {
                        "nanometers": 650.000
                      }
                    }
                  },
                  "targetTimes": [
                    {
                      "selected": {
                        "exposureCount": 10,
                        "exposureTime": {
                          "seconds": 2.000000
                        }
                      }
                    }
                  ]
                }
              ]
            }
          }
        }
      """
    )

  test("imaging multiple GMOS modes preserve input order"):
    query(
      """
        query {
          imaging(input: {
            exposureTimeMode: {
              timeAndCount: {
                time: {
                  seconds: 1
                },
                count: 2,
                at: { nanometers: 800 }
              }
            },
            asterism: [
              {
                sourceProfile: {
                  point: {
                    bandNormalized: {
                      sed: {
                        stellarLibrary: G5_V
                      }
                      brightnesses: [ {
                        band: I
                        value: 15
                        units: VEGA_MAGNITUDE
                      }]
                    }
                  }
                },
                radialVelocity: {
                  kilometersPerSecond: 0
                }
              }
            ],
            constraints: {
              imageQuality: POINT_EIGHT,
              cloudExtinction: POINT_ONE,
              skyBackground: BRIGHT,
              waterVapor: WET,
              elevationRange: {
                airMass: {
                  min: 1,
                  max: 1.5
                }
              }
            },
            modes: [
              {
                gmosNImaging: {
                  filter: G_PRIME
                }
              },
              {
                gmosNImaging: {
                  filter: R_PRIME
                }
              }
            ]
          }) {
            all {
              mode {
                instrument
                ... on ImagingMode {
                  params {
                    ... on GmosNImagingParams {
                      filter
                    }
                  }
                }
              }
              targetTimes {
                ... on TargetIntegrationTime {
                  selected {
                    exposureCount
                    exposureTime {
                      seconds
                    }
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
            "imaging": {
              "all": [
                {
                  "mode": {
                    "instrument": "GMOS_NORTH",
                    "params": {
                      "filter": "G_PRIME"
                    }
                  },
                  "targetTimes": [
                    {
                      "selected": {
                        "exposureCount": 10,
                        "exposureTime": {
                          "seconds": 1.000000
                        }
                      }
                    }
                  ]
                },
                {
                  "mode": {
                    "instrument": "GMOS_NORTH",
                    "params": {
                      "filter": "R_PRIME"
                    }
                  },
                  "targetTimes": [
                    {
                      "selected": {
                        "exposureCount": 10,
                        "exposureTime": {
                          "seconds": 1.000000
                        }
                      }
                    }
                  ]
                }
              ]
            }
          }
        }
      """
    )

