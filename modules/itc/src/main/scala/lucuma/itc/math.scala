// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.math

import cats.syntax.all._
import lucuma.itc.ItcAxis

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

// Calculate the values on the axis' range
def calcAxis(data: List[(Double, Double)], fun: ((Double, Double)) => Double): Option[ItcAxis] =
  if (data.nonEmpty)
    val (min, max, count) =
      data.foldLeft((Double.MaxValue, Double.MinValue, 0)) { case ((max, min, count), current) =>
        val x = fun(current)
        (x.min(max), x.max(min), count + 1)
      }
    ItcAxis(fun(data.head), fun(data.last), min, max, count).some
  else none
