// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input
package sourceprofile

import cats.syntax.all.*
import grackle.Result
import lucuma.itc.input.customSed.CustomSed
import lucuma.odb.graphql.binding.*

object CustomSedIdInput {

  val Binding: Matcher[CustomSed.Id] =
    ObjectFieldsBinding.rmap {
      case List(
            ProgramIdBinding("programId", rProgramId),
            AttachmentIdBinding("attachmentId", rAttachmentId)
          ) =>
        (rProgramId, rAttachmentId).parMapN(CustomSed.Id(_, _))
    }
}
