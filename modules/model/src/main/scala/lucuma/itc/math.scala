// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.math

import scala.math._

// Round the value to n significant figures, this to remove
// the extra precision produced by ITC calculations using doubles
// This will not work for extremely small numbers but we don't expect such
def roundToSignificantFigures(num: Double, n: Int): Double =
  if num == 0 then 0
  else
    val d     = ceil(log10(abs(num)))
    val power = n - d.toInt

    val magnitude = pow(10, power)
    val shifted   = round(num * magnitude)
    shifted / magnitude
