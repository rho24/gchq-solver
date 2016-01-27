namespace PuzzleSolversWeb.Controllers.Api
open System
open System.Collections.Generic
open System.Linq
open System.Net.Http
open System.Web.Http
open common
open Puzzle
open solver

type AxisChange = 
  { Orientation : string
    Index : int
    NewAxisState : string list }

type PuzzleVM =
  { PuzzleStart : string[,]
    Height : int
    Width : int
    Changes : AxisChange list
  }
/// Retrieves values.
[<RoutePrefix("api/gchq")>]
type GchqPuzzleController() =
  inherit ApiController()

  /// Gets all values.
  [<Route("")>]
  member x.Get() =
    let original = convertPatternToResult Puzzle.initialGrid
    let result = solvePuzzle original Puzzle.rowTests Puzzle.colTests
    let prettyCellResult = fun e -> match e with
                                    | B -> "B"
                                    | W -> "W"
                                    | U -> "U"

    let prettyAxis = fun a -> match a with | Row -> "Row" | Column -> "Column"

    let prettyOrig =
      original
      |> Array2D.map prettyCellResult

    let prettyChanges =
      result.AxisChanges
      |> List.rev
      |> List.map (fun c -> 
      {Orientation = prettyAxis c.Orientation
       Index = c.Index 
       NewAxisState = (List.map prettyCellResult c.NewAxisState)})
    {PuzzleStart = prettyOrig
     Height = Array2D.length1 original
     Width = Array2D.length2 original
     Changes = prettyChanges}