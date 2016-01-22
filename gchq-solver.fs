module gchq

open System
open System.IO
open System.Text
open System.Threading
open solver
open Fuchu
open PuzzleTests

let guessPuzzle = 
  fun () -> 
    let origPosition = (Console.CursorLeft, Console.CursorTop)
    let start = convertPatternToResult Puzzle.initialGrid
    let width = Array2D.length2 start
    let height = Array2D.length1 start
    let possiblesForRows = Puzzle.rowTests |> List.map (fun x -> getPossibles width x)
    printfn "Calculated row possibles"
    let possiblesForColumns = Puzzle.colTests |> List.map (fun x -> getPossibles height x)
    printfn "Calculated column possibles"
    let mutable guess = start
    seq { 0..100 }
    |> Seq.tryFind (fun i -> 
         guess <- rows guess
                  |> List.map3 trySolveAxis
                  <| Puzzle.rowTests
                  <| possiblesForRows
                  |> array2D
         Console.SetCursorPosition origPosition
         printResultGrid guess Puzzle.rowTests Puzzle.colTests |> Console.WriteLine
         guess <- columns guess
                  |> List.map3 trySolveAxis
                  <| Puzzle.colTests
                  <| possiblesForColumns
                  |> array2D
                  |> rotate
         Console.SetCursorPosition origPosition
         printResultGrid guess Puzzle.rowTests Puzzle.colTests |> Console.WriteLine
         let finished = rows guess |> List.forall (fun r -> List.forall (fun c -> c <> U) r)
         if finished then printfn "\nFinished after %i" i
         finished)
    |> ignore

[<EntryPoint>]
let main argv = 
  run tests |> ignore
  run brokenTests |> ignore
  guessPuzzle()
  0 // return an integer exit code
