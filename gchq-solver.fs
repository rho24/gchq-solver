module gchq

open System
open System.IO
open System.Threading
open utility
open common
open possibles
open solver
open Fuchu
open PuzzleTests

let printResultGrid (grid) (rowTests : int list list) (columnTests : int list list) = 
  use output = new StringWriter()
  let rowResults = List.map2 testAxis (rows grid) rowTests
  let columnResults = List.map2 testAxis (columns grid) columnTests
  fprintf output "     "
  for result in columnResults do
    match result with
    | Pass -> fprintf output "Y "
    | Fail -> fprintf output "N "
    | Indeterminate -> fprintf output "? "
  fprintf output "\n\n   "
  for i in 1..27 do
    fprintf output "\u2588\u2588"
  fprintfn output ""
  for i in 0..(Array2D.length1 grid) - 1 do
    let row = row i grid
    let result = rowResults.[i]
    match result with
    | Pass -> fprintf output "Y  \u2588\u2588"
    | Fail -> fprintf output "N  \u2588\u2588"
    | Indeterminate -> fprintf output "?  \u2588\u2588"
    for j in row do
      match j with
      | U -> fprintf output "??"
      | B -> fprintf output "  "
      | W -> fprintf output "\u2588\u2588"
    fprintf output "\u2588\u2588\n"
  fprintf output "   "
  for i in 1..27 do
    fprintf output "\u2588\u2588"
  output.ToString()

let guessPuzzle = 
  fun () -> 
    let start = convertPatternToResult Puzzle.initialGrid
    let width = Array2D.length2 start
    let height = Array2D.length1 start
    let possiblesForRows = Puzzle.rowTests |> List.map (fun x -> ((width, x), getPossibles width x))
    printfn "Calculated row possibles"
    let possiblesForColumns = Puzzle.colTests |> List.map (fun x -> ((height, x), getPossibles height x))
    printfn "Calculated column possibles"
    let possiblesCache = List.append possiblesForRows possiblesForColumns |> Map.ofList
    let origPosition = (Console.CursorLeft, Console.CursorTop)
    
    let mutable currentState = 
      { RowTests = Puzzle.rowTests
        ColumnTests = Puzzle.colTests
        Cells = start
        AxisChanges = [] }
    
    let finishedIndex = 
      seq { 0..1000 } |> Seq.tryFind (fun i -> 
                           let testIndex = i % (width + height)
                           
                           let orientation = 
                             if testIndex < height then Row
                             else Column
                           
                           let index = 
                             match orientation with
                             | Row -> testIndex
                             | Column -> testIndex - height
                           
                           currentState <- tryImproveSolution currentState possiblesCache orientation index
                           Console.SetCursorPosition origPosition
                           printfn "%A\t%d " orientation index
                           printResultGrid currentState.Cells Puzzle.rowTests Puzzle.colTests |> Console.WriteLine
                           let finished = 
                             rows currentState.Cells |> List.forall (fun r -> List.forall (fun c -> c <> U) r)
                           finished)
    
    match finishedIndex with
    | None -> printfn "Failed to complete" |> ignore
    | Some i -> printfn "Complete on index %i" i |> ignore

[<EntryPoint>]
let main argv = 
  run tests |> ignore
  run brokenTests |> ignore
  guessPuzzle()
  0 // return an integer exit code
