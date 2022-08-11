// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import cats.syntax.all.*
import io.circe.Error
import io.circe.parser.decode
import io.circe.syntax.*

class ItcChartSuite extends munit.FunSuite {
  val json = """{
        "charts" : [
          {
            "series" : [
              {
                "dataType" : {
                  "SignalData" : {

                  }
                },
                "title" : "Signal BB(B)",
                "data" : [
                  [
                    518.386,
                    518.4119999999999,
                    518.438,
                    518.4639999999999,
                    518.49
                  ],
                  [
                    0.0,
                    1648773.279057645,
                    1648738.5069462687,
                    1648686.1824108209,
                    1648543.5849223256
                  ]
                ]
              },
              {
                "dataType" : {
                  "BackgroundData" : {

                  }
                },
                "title" : "SQRT(Background) BB(B)",
                "data" : [
                  [
                    518.386,
                    518.4119999999999,
                    518.438,
                    518.4639999999999,
                    518.49
                  ],
                  [
                    0.0,
                    0.2232938682240929,
                    0.22338510409116039,
                    0.2234823329111075,
                    0.22357480860414133
                  ]
                ]
              }
            ]
          }
        ]
      }"""

  val chart = ItcChart(
    List(
      ItcSeries(
        "Signal BB(B)",
        SeriesDataType.SignalData,
        List((518.386, 0.0),
             (518.4119999999999, 1648773.279057645),
             (518.438, 1648738.5069462687),
             (518.4639999999999, 1648686.1824108209),
             (518.49, 1648543.5849223256)
        )
      ),
      ItcSeries(
        "SQRT(Background) BB(B)",
        SeriesDataType.BackgroundData,
        List(
          (518.386, 0.0),
          (518.4119999999999, 0.2232938682240929),
          (518.438, 0.22338510409116039),
          (518.4639999999999, 0.2234823329111075),
          (518.49, 0.22357480860414133)
        )
      )
    )
  )

  // pprint.pprintln(decode[ItcChart](json))
  // pprint.pprintln(chart.asJson.spaces2)

  val encoded = """{
  "series" : [
    {
      "title" : "Signal BB(B)",
      "dataType" : "SIGNAL_DATA",
      "data" : [
        [
          518.386,
          0.0
        ],
        [
          518.4119999999999,
          1648773.279057645
        ],
        [
          518.438,
          1648738.5069462687
        ],
        [
          518.4639999999999,
          1648686.1824108209
        ],
        [
          518.49,
          1648543.5849223256
        ]
      ]
    },
    {
      "title" : "SQRT(Background) BB(B)",
      "dataType" : "BACKGROUND_DATA",
      "data" : [
        [
          518.386,
          0.0
        ],
        [
          518.4119999999999,
          0.2232938682240929
        ],
        [
          518.438,
          0.22338510409116039
        ],
        [
          518.4639999999999,
          0.2234823329111075
        ],
        [
          518.49,
          0.22357480860414133
        ]
      ]
    }
  ]
}""".stripMargin

  test("decode graph") {
    assertEquals(chart.asRight, decode[ItcChart](json))
  }

  test("encode graph") {
    assertEquals(chart.asJson.spaces2, encoded.trim)
  }
}
