// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.search

package object syntax {
  object all
      extends ToGmosNorthFilterOps
      with ToGmosNorthGratingOps
      with ToGmosNorthFpuOps
      with ToGmosSouthFilterOps
      with ToGmosSouthGratingOps
      with ToGmosSouthFpuOps
      with lucuma.itc.legacy.syntax.ConditionsSyntax

  object sed
      extends ToStellarLibrarySpectrumOps
      with ToGalaxySpectrumOps
      with ToPlanetSpectrumOps
      with ToHIIRegionSpectrumOps
      with ToPlanetaryNebulaSpectrumOps
      with ToQuasarSpectrumOps
      with ToCoolStarModelOps
}
