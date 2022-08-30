// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service.redis

import boopickle.DefaultBasic.*
import cats.data.NonEmptyList
import eu.timepit.refined.*
import eu.timepit.refined.api.*
import lucuma.core.util.Enumerated
import lucuma.itc.*

import scala.concurrent.duration.*

// --------------------------------------------------
// Pickler to store results in binary with boopickle
// The data is further gzipped
// --------------------------------------------------

given picklerRefined[A: Pickler, B](using Validate[A, B]): Pickler[A Refined B] =
  new Pickler[A Refined B] {
    override def pickle(a: A Refined B)(using state: PickleState): Unit = {
      state.pickle(a.value)
      ()
    }
    override def unpickle(using state: UnpickleState): A Refined B      = {
      val value = state.unpickle[A]
      refineV[B](value).getOrElse(sys.error("Cannot unpickle"))
    }
  }

given picklerEnumerated[A: Enumerated]: Pickler[A] =
  transformPickler((a: String) => Enumerated[A].fromTag(a).getOrElse(sys.error("Cannot unpickle")))(
    Enumerated[A].tag(_)
  )

given picklerNonEmptyList[A: Pickler]: Pickler[NonEmptyList[A]] =
  transformPickler(NonEmptyList.fromListUnsafe[A])(_.toList)

given Pickler[ItcSeries]      =
  transformPickler(Function.tupled(ItcSeries.apply _))(x => (x.title, x.seriesType, x.data))
given Pickler[FiniteDuration] =
  transformPickler(n => new FiniteDuration(n, NANOSECONDS))(_.toNanos)

given Pickler[ItcChart]                        = generatePickler
given Pickler[ItcChartGroup]                   = generatePickler
given Pickler[ItcWarning]                      = generatePickler
given Pickler[ItcCcd]                          = generatePickler
given Pickler[Itc.GraphResult]                 = generatePickler
given Pickler[Itc.CalcResult.Success]          = generatePickler
given Pickler[Itc.CalcResult.SourceTooBright]  = generatePickler
given Pickler[Itc.CalcResult.CalculationError] = generatePickler
given Pickler[Itc.CalcResult]                  = compositePickler[Itc.CalcResult]
  .addConcreteType[Itc.CalcResult.Success]
  .addConcreteType[Itc.CalcResult.SourceTooBright]
  .addConcreteType[Itc.CalcResult.CalculationError]
given Pickler[Itc.CalcResultWithVersion]       = generatePickler
