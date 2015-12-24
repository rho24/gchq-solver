module gchq

open System
open System.IO
open System.Text
open System.Threading

open solver
open Fuchu
open PuzzleTests

[<EntryPoint>]
let main argv =
  
  let origPosition = (Console.CursorLeft, Console.CursorTop)
  
  let start = convertPatternToResult Puzzle.initialGrid
    
//  run tests |> ignore
  let mutable guess = start
  for i in 1..100 do
    Thread.Sleep(100)
    guess <- 
      rows guess
      |> List.map2 trySolveAxis <| Puzzle.rowTests
      |> array2D
    
    guess <-
      columns guess
      |> List.map2 trySolveAxis <| Puzzle.colTests
      |> array2D
      |> rotate
    
    Console.SetCursorPosition origPosition
    printResultGrid guess Puzzle.rowTests Puzzle.colTests |> Console.WriteLine
    
  0 // return an integer exit code
