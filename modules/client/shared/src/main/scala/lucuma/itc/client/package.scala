// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client

import clue.ErrorPolicy

given DefaultErrorPolicy: ErrorPolicy.RaiseOnNoData.type = ErrorPolicy.RaiseOnNoData
