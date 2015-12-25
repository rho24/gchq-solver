module gchq

open System
open System.IO
open System.Text
open System.Threading

open solver
open Fuchu
open PuzzleTests

let guessPuzzle = fun () ->
  let origPosition = (Console.CursorLeft, Console.CursorTop)
  
  let start = convertPatternToResult Puzzle.initialGrid
  let mutable guess = start
  for i in 1..100 do
    Thread.Sleep(1000)
    guess <- 
      rows guess
      |> List.map2 trySolveAxis <| Puzzle.rowTests
      |> array2D
    
    Console.SetCursorPosition origPosition
    printResultGrid guess Puzzle.rowTests Puzzle.colTests |> Console.WriteLine

    guess <-
      columns guess
      |> List.map2 trySolveAxis <| Puzzle.colTests
      |> array2D
      |> rotate
    
    Console.SetCursorPosition origPosition
    printResultGrid guess Puzzle.rowTests Puzzle.colTests |> Console.WriteLine
    
[<EntryPoint>]
let main argv =
  
    
  run tests |> ignore
  run brokenTests |> ignore

  guessPuzzle ()

  0 // return an integer exit code
