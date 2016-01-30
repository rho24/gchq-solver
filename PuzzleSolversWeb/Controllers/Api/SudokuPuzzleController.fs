namespace PuzzleSolversWeb.Controllers.Api

open System
open System.Collections.Generic
open System.Linq
open System.Net.Http
open System.Web.Http
open Sudoku

module Sudoku = 
  type AxisChange = 
    { Orientation : string
      Index : int
      NewAxisState : string list }
  
  type CellVM = 
    { cellType : string
      value : int
      possibles : int list }
  
  type PuzzleVM = CellVM [,]
  
  type AuditVM = 
    { message : string
      puzzle : PuzzleVM }
  
  type SolutionVM = 
    { puzzle : PuzzleVM
      audit : AuditVM list }
  
  let mapPuzzle (Puzzle cells) = 
    cells |> Array2D.map (function 
               | Value v -> 
                 { cellType = "Value"
                   value = Puzzle.valueToInt v
                   possibles = [] }
               | Possibles p -> 
                 { cellType = "Possibles"
                   value = 0
                   possibles = p |> List.map Puzzle.valueToInt })
  
  /// Retrieves values.
  [<RoutePrefix("api/sudoku")>]
  type SudokuPuzzleController() = 
    inherit ApiController()
    /// Gets all values.
    [<Route("")>]
    member x.Get() = 
      let pHard = 
        Puzzle.parseList [ [ 6; 0; 0; 2; 0; 0; 0; 4; 0 ]
                           [ 3; 0; 0; 0; 0; 0; 0; 0; 8 ]
                           [ 0; 0; 0; 1; 0; 0; 6; 5; 9 ]
                           [ 4; 0; 0; 3; 0; 0; 0; 0; 7 ]
                           [ 0; 1; 3; 0; 9; 0; 0; 0; 0 ]
                           [ 0; 7; 0; 0; 0; 0; 5; 0; 0 ]
                           [ 2; 0; 0; 0; 1; 0; 0; 0; 0 ]
                           [ 0; 3; 0; 0; 0; 4; 0; 0; 0 ]
                           [ 0; 5; 0; 6; 7; 0; 0; 0; 0 ] ]
      
      let solution = pHard |> Solver.solve
      { puzzle = solution.puzzle |> mapPuzzle
        audit = 
          solution.audit |> List.map (fun (s, p) -> 
                              { message = s
                                puzzle = p |> mapPuzzle }) }
