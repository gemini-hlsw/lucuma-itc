// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import cats.syntax.all.*
import io.circe.literal.*
import lucuma.core.enums.*
import lucuma.core.syntax.string.*
import lucuma.core.util.Enumerated
import lucuma.itc.ItcObservingConditions

class GraphQLSpectroscopyTimeSuite extends GraphQLSuite {

  test("gmos north case") {
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
            selected {
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
            "spectroscopyIntegrationTime" : {
              "mode" : {
                "instrument" : "GMOS_NORTH",
                "resolution" : 970,
                "params": {
                  "grating": "B1200_G5301"
                },
                "wavelength" : {
                  "nanometers" : 60.000
                }
              },
              "selected" : {
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

  test("gmos south case") {
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
                    planet: JUPITER
                  }
                  brightnesses: [ {
                    band: R
                    value: 3
                    units: ERG_PER_S_PER_CM_SQUARED_PER_A
                    error: 0.2
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
              gmosSSpectroscopy: {
                filter: RG610,
                fpu: {
                  builtin: LONG_SLIT_0_25
                },
                grating: B1200_G5321
              }
            }
          }) {
              mode {
                ... on SpectroscopyMode {
                  instrument
                  resolution
                  params {
                    ... on GmosSITCParams {
                      grating
                    }
                  }
                  wavelength {
                    nanometers
                  }
                }
              }
              selected {
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
            "spectroscopyIntegrationTime" : {
                "mode" : {
                  "instrument" : "GMOS_SOUTH",
                  "resolution" : 970,
                  "params": {
                    "grating": "B1200_G5321"
                  },
                  "wavelength" : {
                    "nanometers" : 60.000
                  }
                },
                "selected" : {
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

  test("gmos north case with variables") {
    query(
      """
        query($spectroscopy: SpectroscopyIntegrationTimeInput) {\n          spectroscopyIntegrationTime(input: $spectroscopy) {\n            mode {\n ... on SpectroscopyMode {\n                instrument\n              }\n       }\n            }\n        }\n
      """,
      """
        {
          "spectroscopy" : {
            "wavelength" : {
              "nanometers" : "600"
            },
            "signalToNoise" : 2,
            "sourceProfile": {
              "uniform": {
                "bandNormalized": {
                  "sed": {
                    "stellarLibrary": "O5_V"
                  },
                  "brightnesses": [ {
                    "band": "R",
                    "value": "3",
                    "units": "VEGA_MAG_PER_ARCSEC_SQUARED"
                  }, {
                    "band": "K",
                    "value": "2.1",
                    "units": "W_PER_M_SQUARED_PER_UM_PER_ARCSEC_SQUARED"
                  }]
                }
              }
            },
            "band": "J",
            "radialVelocity": {
              "metersPerSecond": 1000
            },
            "constraints" : {
              "imageQuality" : "POINT_EIGHT",
              "cloudExtinction" : "POINT_FIVE",
              "skyBackground" : "DARK",
              "waterVapor" : "DRY",
              "elevationRange" : {
                "airMass": {
                  "min": "1.1",
                  "max": "1.3"
                }
              }
            },
            "mode": {
              "gmosNSpectroscopy": {
                "filter": "G_PRIME",
                "fpu": {
                  "builtin": "LONG_SLIT_0_25"
                },
                "grating": "B1200_G5301"
              }
            }
          }
        }
        """,
      json"""
        {
          "data": {
            "spectroscopyIntegrationTime" :
              {
                    "mode" : {
                      "instrument" : "GMOS_NORTH"
                    }
              }
          }
        }
        """
    )
  }

  val allConditions =
    for {
      iq <- Enumerated[ImageQuality].all
      ce <- Enumerated[CloudExtinction].all
      wv <- Enumerated[WaterVapor].all
      sb <- Enumerated[SkyBackground].all
    } yield ItcObservingConditions(iq, ce, wv, sb, 2)

  val conditions = ItcObservingConditions(ImageQuality.PointEight,
                                          CloudExtinction.OnePointFive,
                                          WaterVapor.Median,
                                          SkyBackground.Bright,
                                          2
  )

  test("iterate over conditions") {
    allConditions.traverse { c =>
      query(
        s"""
        query {
          spectroscopyIntegrationTime(input: {
            wavelength: {
              nanometers: 60,
            },
            radialVelocity: {
              centimetersPerSecond: 1000
            },
            signalToNoise: 2,
            sourceProfile: {
              uniform: {
                bandNormalized: {
                  sed: {
                    powerLaw: 3.0
                  },
                  brightnesses: [ {
                    band: R,
                    value: 3,
                    units: VEGA_MAG_PER_ARCSEC_SQUARED
                  }, {
                    band: K,
                    value: 2.1,
                    units: W_PER_M_SQUARED_PER_UM_PER_ARCSEC_SQUARED
                  }]
                }
              }
            },
            band: K
            constraints: {
              imageQuality: ${c.iq.tag.toScreamingSnakeCase},
              cloudExtinction: ${c.cc.tag.toScreamingSnakeCase},
              skyBackground: ${c.sb.tag.toScreamingSnakeCase},
              waterVapor: ${c.wv.tag.toScreamingSnakeCase},
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
                selected {
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
            "spectroscopyIntegrationTime" :
              {
                    "mode" : {
                      "instrument" : "GMOS_NORTH",
                      "resolution" : 970,
                      "params": {
                        "grating": "B1200_G5301"
                      },
                      "wavelength" : {
                        "nanometers" : 60.000
                      }
                    },
                    "selected" : {
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

  test("Bad airmass") {
    query(
      """
        query {
          spectroscopyIntegrationTime(input: {
            wavelength: {
              nanometers: 60,
            },
            radialVelocity: {
              metersPerSecond: 1000
            },
            signalToNoise: 2,
            sourceProfile: {
              uniform: {
                bandNormalized: {
                  sed: {
                    blackBodyTempK: 100
                  },
                  brightnesses: [ {
                    band: R,
                    value: 3,
                    units: VEGA_MAG_PER_ARCSEC_SQUARED
                  }, {
                    band: K,
                    value: 2.1,
                    units: W_PER_M_SQUARED_PER_UM_PER_ARCSEC_SQUARED
                  }]
                }
              }
            },
            band: K
            constraints: {
              imageQuality: POINT_THREE,
              cloudExtinction: POINT_FIVE,
              skyBackground: DARK,
              waterVapor: DRY,
              elevationRange: {
                airMass: {
                  min: 2,
                  max: 1
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
                selected {
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
          "data": null,
          "errors": [
            {
              "message" : "Creating an air mass range requires specifying both min and max where min < max"
            }
          ]
        }
        """
    )
  }

  test("gmosN_gratings") {
    Enumerated[GmosNorthGrating].all.traverse { d =>
      query(
        s"""
        query {
          spectroscopyIntegrationTime(input: {
            wavelength: {
              nanometers: 60,
            },
            radialVelocity: {
              centimetersPerSecond: 1000
            },
            signalToNoise: 2,
            sourceProfile: {
              uniform: {
                bandNormalized: {
                  sed: {
                    blackBodyTempK: 100
                  },
                  brightnesses: [ {
                    band: R,
                    value: 3,
                    units: VEGA_MAG_PER_ARCSEC_SQUARED
                  }, {
                    band: K,
                    value: 2.1,
                    units: W_PER_M_SQUARED_PER_UM_PER_ARCSEC_SQUARED
                  }]
                }
              }
            },
            band: K
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
                grating: ${d.tag.toScreamingSnakeCase}
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
                    wavelength {
                      nanometers
                    }
                  }
                }
                selected {
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
            "spectroscopyIntegrationTime" :
              {
                    "mode" : {
                      "instrument" : "GMOS_NORTH",
                      "params": {
                        "grating": ${d.tag.toScreamingSnakeCase}
                      },
                      "wavelength" : {
                        "nanometers" : 60.00
                      }
                    },
                    "selected" : {
                      "exposures" : 10,
                      "exposureTime" : {
                        "seconds" : 1
                      }
                    }
              }
          }
        }
        """
      )
    }
  }

  test("gmosS_gratings") {
    Enumerated[GmosSouthGrating].all.traverse { d =>
      query(
        s"""
        query {
          spectroscopyIntegrationTime(input: {
            wavelength: {
              nanometers: 60,
            },
            radialVelocity: {
              centimetersPerSecond: 1000
            },
            signalToNoise: 2,
            sourceProfile: {
              gaussian: {
                fwhm: {
                  microarcseconds: 100
                }
                spectralDefinition: {
                  bandNormalized: {
                    sed: {
                      blackBodyTempK: 100
                    },
                    brightnesses: [ {
                      band: R,
                      value: 3,
                      units: VEGA_MAGNITUDE
                    }, {
                      band: K,
                      value: 2.1,
                      units: JANSKY
                    }]
                  }
                }
              }
            },
            band: K
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
              gmosSSpectroscopy: {
                filter: G_PRIME,
                fpu: {
                  builtin: LONG_SLIT_0_25
                },
                grating: ${d.tag.toScreamingSnakeCase}
              }
            }
          }) {
                mode {
                  ... on SpectroscopyMode {
                    instrument
                    params {
                      ... on GmosSITCParams {
                        grating
                      }
                    }
                    wavelength {
                      nanometers
                    }
                  }
                }
                selected {
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
            "spectroscopyIntegrationTime" :
              {
                    "mode" : {
                      "instrument" : "GMOS_SOUTH",
                      "params": {
                        "grating": ${d.tag.toScreamingSnakeCase}
                      },
                      "wavelength" : {
                        "nanometers" : 60.00
                      }
                    },
                    "selected" : {
                      "exposures" : 10,
                      "exposureTime" : {
                        "seconds" : 1
                      }
                    }
                  }
          }
        }
        """
      )
    }
  }

  test("gmosN_fpu") {
    Enumerated[GmosNorthFpu].all.traverse { d =>
      query(
        s"""
          query {
            spectroscopyIntegrationTime(input: {
              wavelength: {
                nanometers: 60,
              },
              radialVelocity: {
                centimetersPerSecond: 1000
              },
              signalToNoise: 2,
              sourceProfile: {
                gaussian: {
                  fwhm: {
                    microarcseconds: 100
                  }
                  spectralDefinition: {
                    bandNormalized: {
                      sed: {
                        blackBodyTempK: 100
                      },
                      brightnesses: [ {
                        band: R,
                        value: 3,
                        units: VEGA_MAGNITUDE
                      }, {
                        band: K,
                        value: 2.1,
                        units: JANSKY
                      }]
                    }
                  }
                }
              },
              band: AP
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
                    builtin: ${d.tag.toScreamingSnakeCase}
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
                          fpu {
                            builtin
                          }
                        }
                      }
                      wavelength {
                        nanometers
                      }
                    }
                  }
                  selected {
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
          "spectroscopyIntegrationTime" :
            {
                  "mode" : {
                    "instrument" : "GMOS_NORTH",
                    "params": {
                      "fpu": {
                        "builtin": ${d.tag.toScreamingSnakeCase}
                      }
                    },
                    "wavelength" : {
                      "nanometers" : 60.00
                    }
                  },
                  "selected" : {
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

  test("gmosS_fpu") {
    Enumerated[GmosSouthFpu].all.filter(_ =!= GmosSouthFpu.Bhros).traverse { d =>
      query(
        s"""
        query {
          spectroscopyIntegrationTime(input: {
            wavelength: {
              nanometers: 60,
            },
            radialVelocity: {
              centimetersPerSecond: 1000
            },
            signalToNoise: 2,
            sourceProfile: {
              uniform: {
                bandNormalized: {
                  sed: {
                    blackBodyTempK: 100
                  },
                  brightnesses: [ {
                    band: R,
                    value: 3,
                    units: VEGA_MAG_PER_ARCSEC_SQUARED
                  }, {
                    band: K,
                    value: 2.1,
                    units: W_PER_M_SQUARED_PER_UM_PER_ARCSEC_SQUARED
                  }]
                }
              }
            },
            band: K
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
              gmosSSpectroscopy: {
                filter: G_PRIME,
                fpu: {
                  builtin: ${d.tag.toScreamingSnakeCase}
                },
                grating: B1200_G5321
              }
            }
          }) {
                mode {
                  ... on SpectroscopyMode {
                    instrument
                    params {
                      ... on GmosSITCParams {
                        fpu {
                          builtin
                        }
                      }
                    }
                    wavelength {
                      nanometers
                    }
                  }
                }
                selected {
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
            "spectroscopyIntegrationTime" :
              {
                    "mode" : {
                      "instrument" : "GMOS_SOUTH",
                      "params": {
                        "fpu": {
                          "builtin": ${d.tag.toScreamingSnakeCase}
                        }
                      },
                      "wavelength" : {
                        "nanometers" : 60.00
                      }
                    },
                    "selected" : {
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

  test("gmosN_filter") {
    Enumerated[GmosNorthFilter].all.traverse { d =>
      query(
        s"""
        query {
          spectroscopyIntegrationTime(input: {
            wavelength: {
              nanometers: 60,
            },
            radialVelocity: {
              centimetersPerSecond: 1000
            },
            signalToNoise: 2,
            sourceProfile: {
              uniform: {
                bandNormalized: {
                  sed: {
                    blackBodyTempK: 100
                  },
                  brightnesses: [ {
                    band: R,
                    value: 3,
                    units: VEGA_MAG_PER_ARCSEC_SQUARED
                  }, {
                    band: K,
                    value: 2.1,
                    units: W_PER_M_SQUARED_PER_UM_PER_ARCSEC_SQUARED
                  }]
                }
              }
            },
            band: K
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
                filter: ${d.tag.toScreamingSnakeCase}
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
                        filter
                      }
                    }
                    wavelength {
                      nanometers
                    }
                  }
                }
                selected {
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
            "spectroscopyIntegrationTime" :
              {
                    "mode" : {
                      "instrument" : "GMOS_NORTH",
                      "params": {
                        "filter": ${d.tag.toScreamingSnakeCase}
                      },
                      "wavelength" : {
                        "nanometers" : 60.000
                      }
                    },
                    "selected" : {
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

  test("gmosS_filter") {
    Enumerated[GmosSouthFilter].all.traverse { d =>
      query(
        s"""
        query {
          spectroscopyIntegrationTime(input: {
            wavelength: {
              nanometers: 60,
            },
            radialVelocity: {
              centimetersPerSecond: 1000
            },
            signalToNoise: 2,
            sourceProfile: {
              uniform: {
                bandNormalized: {
                  sed: {
                    blackBodyTempK: 100
                  },
                  brightnesses: [ {
                    band: R,
                    value: 3,
                    units: VEGA_MAG_PER_ARCSEC_SQUARED
                  }, {
                    band: K,
                    value: 2.1,
                    units: W_PER_M_SQUARED_PER_UM_PER_ARCSEC_SQUARED
                  }]
                }
              }
            },
            band: K
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
              gmosSSpectroscopy: {
                filter: ${d.tag.toScreamingSnakeCase}
                fpu: {
                  builtin: LONG_SLIT_0_25
                },
                grating: B1200_G5321
              }
            }
          }) {
                mode {
                  ... on SpectroscopyMode {
                    instrument
                    params {
                      ... on GmosSITCParams {
                        filter
                      }
                    }
                    wavelength {
                      nanometers
                    }
                  }
                }
                selected {
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
            "spectroscopyIntegrationTime" :
              {
                    "mode" : {
                      "instrument" : "GMOS_SOUTH",
                      "params": {
                        "filter": ${d.tag.toScreamingSnakeCase}
                      },
                      "wavelength" : {
                        "nanometers" : 60.000
                      }
                    },
                    "selected" : {
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
}
