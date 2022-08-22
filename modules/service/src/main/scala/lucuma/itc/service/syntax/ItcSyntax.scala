// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service.syntax

import cats.data._
import edu.gemini.grackle.Problem
import edu.gemini.grackle.Query.Environment
import lucuma.itc.ItcCcd
import lucuma.itc.ItcChart
import lucuma.itc.ItcSeries
import lucuma.itc.SignificantFigures
import lucuma.itc.math.roundToSignificantFigures
import monocle.Focus
import monocle.std.these._

trait ItcSyntax:

  extension [A](self: Option[A])
    def toRightIorNec[E](e: => E): Ior[NonEmptyChain[E], A] =
      self match
        case Some(a) => Ior.Right(a)
        case None    => Ior.Left(NonEmptyChain.of(e))

  extension [A](self: A)
    def leftIorNec[B]: Ior[NonEmptyChain[A], B] =
      Ior.Left(NonEmptyChain.of(self))

    def rightIorNec[E]: Ior[NonEmptyChain[E], A] =
      Ior.Right(self)

  extension [A](self: IorNec[Problem, A])
    def addProblem(problem: String): Ior[NonEmptyChain[Problem], A] =
      self.addLeft(NonEmptyChain.of(Problem(problem)))

  extension [A](self: IorNec[String, A])
    def leftProblems: Ior[NonEmptyChain[Problem], A] =
      self.leftMap(_.map(Problem(_)))

  extension (self: String)
    def fromScreamingSnakeCase: String =
      self.split("_").map(_.toLowerCase.capitalize).mkString("")

  def cursorEnv[A] = theseRight[A, Environment].andThen(Focus[Environment](_.env))

  def cursorEnvAdd[A, B](key: String, value: B): Ior[A, Environment] => Ior[A, Environment] =
    cursorEnv[A].modify(_.add((key, value)))

end ItcSyntax

trait ItcChartSyntax:
  extension (series: ItcSeries)
    def adjustSignificantFigures(figures: SignificantFigures): ItcSeries =
      val data =
        series.data.map((x, y) =>
          (figures.xAxis.fold(x)(xDigits => roundToSignificantFigures(x, xDigits.value)),
           figures.yAxis.fold(y)(yDigits => roundToSignificantFigures(y, yDigits.value))
          )
        )
      ItcSeries(series.title, series.dataType, data)

  extension (chart: ItcChart)
    def adjustSignificantFigures(figures: SignificantFigures): ItcChart =
      chart.copy(series = chart.series.map(_.adjustSignificantFigures(figures)))

  extension (ccd: ItcCcd)
    def adjustSignificantFigures(figures: SignificantFigures): ItcCcd =
      figures.ccd.fold(ccd)(c =>
        ccd.copy(
          singleSNRatio = roundToSignificantFigures(ccd.singleSNRatio, c.value),
          totalSNRatio = roundToSignificantFigures(ccd.totalSNRatio, c.value),
          peakPixelFlux = roundToSignificantFigures(ccd.peakPixelFlux, c.value),
          wellDepth = roundToSignificantFigures(ccd.wellDepth, c.value),
          ampGain = roundToSignificantFigures(ccd.ampGain, c.value)
        )
      )

object all extends ItcSyntax with ItcChartSyntax
