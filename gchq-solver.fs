module gchq

open System
open System.IO
open System.Text
open System.Threading
open solver
open Fuchu
open PuzzleTests

type Axis = 
  | Row
  | Column

type AxisChange = 
  { Orientation : Axis
    Index : int
    NewAxisState : CellResult list }

type PuzzleState = 
  { RowTests : int list list
    ColumnTests : int list list
    Cells : CellResult [,]
    AxisChanges : AxisChange list }

let tryImproveSolution (state : PuzzleState) (possiblesCache : Map<int * int list, CellResult list list>) 
    (orientation : Axis) (index : int) = 
  match orientation with
  | Row -> 
    let currentRowState = row index state.Cells
    let test = state.RowTests.[index]
    let possibles = possiblesCache.Item(currentRowState.Length, test)
    let newRowState = trySolveAxis currentRowState test possibles
    match currentRowState = newRowState with
    | true -> state
    | false -> 
      let change = 
        { Orientation = orientation
          Index = index
          NewAxisState = newRowState }
      { state with Cells = 
                     rows state.Cells
                     |> List.mapi (fun i r -> 
                          if i <> index then r
                          else newRowState)
                     |> array2D
                   AxisChanges = change :: state.AxisChanges }
  | Column -> 
    let currentColumnState = column index state.Cells
    let test = state.ColumnTests.[index]
    let possibles = possiblesCache.Item(currentColumnState.Length, test)
    let newColumnState = trySolveAxis currentColumnState test possibles
    match currentColumnState = newColumnState with
    | true -> state
    | false -> 
      let change = 
        { Orientation = orientation
          Index = index
          NewAxisState = newColumnState }
      { state with Cells = 
                     columns state.Cells
                     |> List.mapi (fun i r -> 
                          if i <> index then r
                          else newColumnState)
                     |> array2D
                     |> rotate
                   AxisChanges = change :: state.AxisChanges }

let guessPuzzle = 
  fun () -> 
    let start = convertPatternToResult Puzzle.initialGrid
    let width = Array2D.length2 start
    let height = Array2D.length1 start
    let possiblesForRows = Puzzle.rowTests |> List.mapi (fun i x -> ((width, x), getPossibles width x))
    printfn "Calculated row possibles"
    let possiblesForColumns = Puzzle.colTests |> List.mapi (fun i x -> ((height, x), getPossibles height x))
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
