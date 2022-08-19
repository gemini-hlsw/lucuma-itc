// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.search.syntax

object all
    extends ToGmosNorthFilterOps
    with ToGmosNorthGratingOps
    with ToGmosNorthFpuOps
    with ToGmosSouthFilterOps
    with ToGmosSouthGratingOps
    with ToGmosSouthFpuOps
    with lucuma.itc.legacy.syntax.ConditionsSyntax
    with lucuma.itc.legacy.syntax.CoolStarModelSyntax
    with lucuma.itc.legacy.syntax.StellarLibrarySpectrumSyntax
    with lucuma.itc.legacy.syntax.GalaxySpectrumSyntax
    with lucuma.itc.legacy.syntax.PlanetSpectrumSyntax
    with lucuma.itc.legacy.syntax.HIIRegionSpectrumSyntax
    with lucuma.itc.legacy.syntax.PlanetaryNebulaSpectrumSyntax
    with lucuma.itc.legacy.syntax.QuasarSpectrumSyntax
