namespace Intro

module SIUnits =
  //open FSharp.Data.UnitSystems.SI.UnitSymbols
  open FSharp.Data.UnitSystems.SI.UnitNames

  let addOneMeter x = x + 1<meter>

  type [<Measure>] foot

  let footPerMeter: float<foot/meter> = 0.3048<_>

  let meterToFoot (m: float<meter>): float<foot> =
    footPerMeter * m

  // Kommentare:
  // - werden zur Kompilierzeit entfernet; zur Laufzeit kein Zugriff auf
  //   die Maßeinheit möglich
