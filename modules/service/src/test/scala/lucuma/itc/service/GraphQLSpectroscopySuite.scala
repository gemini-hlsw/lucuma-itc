// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import cats.syntax.all._
import io.circe.literal._
import lucuma.core.enum.CloudExtinction
import lucuma.core.enum.ImageQuality
import lucuma.core.enum.SkyBackground
import lucuma.core.enum.WaterVapor
import lucuma.core.syntax.enumerated._
import lucuma.core.syntax.string._
import lucuma.core.util.Enumerated
import lucuma.itc.ItcObservingConditions

class GraphQLSpectroscopySuite extends GraphQLSuite {

  test("gmos north case") {
    query(
      """
        query {
          spectroscopy(input: {
            wavelength: {
              nanometers: 60,
            },
            radialVelocity: {
              kilometersPerSecond: 1000
            },
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
            },
            constraints: {
              imageQuality: POINT_THREE,
              cloudExtinction: POINT_FIVE,
              skyBackground: DARK,
              waterVapor: DRY,
              elevationRange: {
                airmassRange: {
                  min: 1,
                  max: 2
                }
              }
            },
            modes: [{
              gmosN: {
                filter: G_PRIME,
                fpu: LONG_SLIT_0_25,
                disperser: B1200_G5301
              }
            }, {
              gmosN: {
                filter: GG455,
                fpu: LONG_SLIT_0_25,
                disperser: B1200_G5301
              }
            }
            ]
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
            "spectroscopy" : [
              {
                "results" : [
                  {
                    "mode" : {
                      "instrument" : "GMOS_NORTH",
                      "resolution" : 970,
                      "params": {
                        "disperser": "B1200_G5301"
                      },
                      "wavelength" : {
                        "nanometers" : 60.00
                      }
                    },
                    "itc" : {
                      "exposures" : 10,
                      "exposureTime" : {
                        "seconds" : 1
                      }
                    }
                  }
                ]
              },
              {
                "results" : [
                  {
                    "mode" : {
                      "instrument" : "GMOS_NORTH",
                      "resolution" : 970,
                      "params": {
                        "disperser": "B1200_G5301"
                      },
                      "wavelength" : {
                        "nanometers" : 60.00
                      }
                    },
                    "itc" : {
                      "exposures" : 10,
                      "exposureTime" : {
                        "seconds" : 1
                      }
                    }
                  }
                ]
              }
            ]
          }
        }
        """
    )
  }

  test("gmos north case with variables") {
    query(
      """
        query($input: SpectroscopyModeInput) {\n          spectroscopy(input: $input) {\n            results {\n              mode {\n                instrument\n              }\n            }\n          }\n        }\n
      """,
      """
        {
          "input" : {
            "wavelength" : {
              "nanometers" : "600"
            },
            "signalToNoise" : "2",
            "spatialProfile" : {
              "sourceType" : "POINT_SOURCE",
              "fwhm" : null
            },
            "spectralDistribution" : {
              "stellar": "A0I"
            },
            "magnitude" : {
              "band" : "I",
              "value" : "6",
              "error" : null,
              "system" : "VEGA"
            },
            "radialVelocity": {
              "metersPerSecond": 1000
            },
            "constraints" : {
              "imageQuality" : "POINT_EIGHT",
              "cloudExtinction" : "POINT_FIVE",
              "skyBackground" : "DARK",
              "waterVapor" : "DRY",
              "elevationRange" : {
                "airmassRange": {
                  "min": "0.1",
                  "max": "1.3"
                }
              }
            },
            "modes": [{
              "gmosN": {
                "filter": "G_PRIME",
                "fpu": "LONG_SLIT_0_25",
                "disperser": "B1200_G5301"
              }
            }, {
              "gmosN": {
                "filter": "GG455",
                "fpu": "LONG_SLIT_0_25",
                "disperser": "B1200_G5301"
              }
            }
            ]
          }
        }
        """,
      json"""
        {
          "data": {
            "spectroscopy" : [
              {
                "results" : [
                  {
                    "mode" : {
                      "instrument" : "GMOS_NORTH"
                    }
                  }
                ]
              },
              {
                "results" : [
                  {
                    "mode" : {
                      "instrument" : "GMOS_NORTH"
                    }
                  }
                ]
              }
            ]
          }
        }
        """
    )
  }

  val allConditions =
    Enumerated[ImageQuality].all
      .zip(Enumerated[CloudExtinction].all)
      .zip(Enumerated[WaterVapor].all)
      .zip(Enumerated[SkyBackground].all)
      .map { case (((iq, ce), wv), sb) =>
        ItcObservingConditions(iq, ce, wv, sb, 2)
      }

  test("iterate over conditions") {
    allConditions.traverse { c =>
      query(
        s"""
        query {
          spectroscopy(input: {
            wavelength: {
              nanometers: 60,
            },
            radialVelocity: {
              centimetersPerSecond: 1000
            },
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
            },
            constraints: {
              imageQuality: ${c.iq.tag.toScreamingSnakeCase},
              cloudExtinction: ${c.cc.tag.toScreamingSnakeCase},
              skyBackground: ${c.sb.tag.toScreamingSnakeCase},
              waterVapor: ${c.wv.tag.toScreamingSnakeCase},
              elevationRange: {
                airmassRange: {
                  min: 1,
                  max: 2
                }
              }
            },
            modes: [{
              gmosN: {
                filter: G_PRIME,
                fpu: LONG_SLIT_0_25,
                disperser: B1200_G5301
              }
            }, {
              gmosN: {
                filter: GG455,
                fpu: LONG_SLIT_0_25,
                disperser: B1200_G5301
              }
            }
            ]
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
            "spectroscopy" : [
              {
                "results" : [
                  {
                    "mode" : {
                      "instrument" : "GMOS_NORTH",
                      "resolution" : 970,
                      "params": {
                        "disperser": "B1200_G5301"
                      },
                      "wavelength" : {
                        "nanometers" : 60.00
                      }
                    },
                    "itc" : {
                      "exposures" : 10,
                      "exposureTime" : {
                        "seconds" : 1
                      }
                    }
                  }
                ]
              },
              {
                "results" : [
                  {
                    "mode" : {
                      "instrument" : "GMOS_NORTH",
                      "resolution" : 970,
                      "params": {
                        "disperser": "B1200_G5301"
                      },
                      "wavelength" : {
                        "nanometers" : 60.00
                      }
                    },
                    "itc" : {
                      "exposures" : 10,
                      "exposureTime" : {
                        "seconds" : 1
                      }
                    }
                  }
                ]
              }
            ]
          }
        }
        """
      )
    }
  }

  test("Bad airmass") {
    query(
      """
        query {
          spectroscopy(input: {
            wavelength: {
              nanometers: 60,
            },
            radialVelocity: {
              metersPerSecond: 1000
            },
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
            },
            constraints: {
              imageQuality: POINT_THREE,
              cloudExtinction: POINT_FIVE,
              skyBackground: DARK,
              waterVapor: DRY,
              elevationRange: {
                airmassRange: {
                  min: 2,
                  max: 1
                }
              }
            },
            modes: [{
              gmosN: {
                filter: G_PRIME,
                fpu: LONG_SLIT_0_25,
                disperser: B1200_G5301
              }
            }, {
              gmosN: {
                filter: GG455,
                fpu: LONG_SLIT_0_25,
                disperser: B1200_G5301
              }
            }
            ]
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
          "errors": [
            {
              "message" : "Airmass max value 1 must be more than the min value 2"
            }
          ]
        }
        """
    )
  }
}
