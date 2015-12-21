module gchq

open System
open System.IO
open System.Text
open System.Threading

type group = int list
type groupsIntermediate = { FinishedGroups : group list; CurrentGroup : group}
type CellResult = Unknown | Black | White
type TestResult = Indeterminate | Pass | Fail

let row i (arr: 'T[,]) = arr.[i..i, *] |> Seq.cast<'T> |> Seq.toList
let column i (arr: 'T[,]) = arr.[*, i..i] |> Seq.cast<'T> |> Seq.toList

let rows (arr: 'T[,]) = 
  [ 0.. Array2D.length1 arr - 1 ] |> List.map (fun i -> row i arr)
  
let columns (arr: 'T[,]) = 
  [ 0.. Array2D.length2 arr - 1 ] |> List.map (fun i -> column i arr)

let public foldi fold first source  =
        source 
        |> List.fold(fun (prev,i) c -> (fold i prev c,i + 1)) (first,0)
        |> fst

let rotate (arr: 'T[,]) =
  Array2D.init (Array2D.length2 arr) (Array2D.length1 arr) (fun x y -> arr.[y,x]) 

let convertPatternToResult grid :CellResult[,] =
  Array2D.map (fun v ->
    match v with
    | 1 -> Black
    | _ -> Unknown
    ) grid

let printRow output row =
  for i in row do
    match i with
    | Unknown -> fprintf output "?"
    | Black -> fprintf output "1"
    | White -> fprintf output "0"

let getGroups (axis) =
  foldi (fun index acc elem -> 
    match elem with
    | White -> match List.length acc.CurrentGroup with
                            | 0 -> acc
                            | _ -> {acc with FinishedGroups = acc.CurrentGroup :: acc.FinishedGroups; CurrentGroup = []}
    | _ -> {acc with CurrentGroup = index :: acc.CurrentGroup}
    ) {FinishedGroups = []; CurrentGroup = []} axis
      |> fun acc -> match List.length acc.CurrentGroup with
                      | 0 -> acc.FinishedGroups
                      | _ -> acc.CurrentGroup :: acc.FinishedGroups
  |> List.map (fun l -> List.rev l)
  |> List.rev

let testAxis (axis) (test: int list) =
  let axisGroups = getGroups axis
  let axisGroupCount = List.length axisGroups
  let testCount = List.length test
    
  match (axisGroupCount, testCount) with
  | (a,b) when a > b -> Fail
  | (a,b) when a = b -> 
    match List.forall2 (fun axisGroup testGroup -> 
      (List.length axisGroup) = testGroup) axisGroups test with
    | true -> Pass
    | false -> Fail
    
  | _ -> Indeterminate

let printResultGrid (grid) (rowTests : int list list) (columnTests : int list list) =
  use output = new StringWriter()
  let rowResults = List.map2 testAxis (rows grid) rowTests
  let columnResults = List.map2 testAxis (columns grid) columnTests
  
  fprintf output "   "
  for result in columnResults do
    match result with
    | Pass -> fprintf output "Y"
    | Fail -> fprintf output "N"
    | Indeterminate -> fprintf output "?"
  fprintfn output ""
  fprintfn output ""
  
  for i in 0 .. (Array2D.length1 grid) - 1 do
    let row = row i grid
    
    let result = rowResults.[i]
        
    match result with
    | Pass -> fprintf output "Y  "
    | Fail -> fprintf output "N  "
    | Indeterminate -> fprintf output "?  "
    printRow output row
    fprintfn output ""
  
  output.ToString()

let rnd = Random()
let addRandomGuess grid =
  let x = rnd.Next(0, 25)
  let y = rnd.Next(0, 25)
  let z = if rnd.Next(0, 2) = 0 then Black else White
  Array2D.mapi (fun i j v -> if i = x && j = y then z else v) grid

let trySolveAxis axis (testGroups: int list) =
  let axisGroups = getGroups axis
  let axisGroupCount = List.length axisGroups
  let testCount = List.length testGroups
  
  let result = 
    match testGroups with
    | [] -> List.map (fun r -> CellResult.White) axis
    | _ -> 
      let minimumSolutionWidth = List.sum testGroups + testCount - 1
      match axis.Length - minimumSolutionWidth with
      | overlap when overlap = 0 ->
          testGroups
          |> List.collect (fun groupLength -> CellResult.White :: List.init groupLength (fun _ -> CellResult.Black))
          |> List.tail
      | overlap when List.max testGroups > overlap ->
          List.init (overlap - 1) (fun _ -> CellResult.Unknown)
          @ List.collect (fun groupLength -> 
              match groupLength with
              | groupLength when groupLength > overlap ->
                List.init (groupLength - overlap) (fun _ -> CellResult.Black)
                @ CellResult.White :: List.init overlap (fun _ -> CellResult.Unknown)
              | _ ->
                List.init (groupLength + 1) (fun _ -> CellResult.Unknown)
              ) testGroups
            
      | _ -> axis
      
  result

// let dsadsa = fun () ->
//   let axis = [
//     CellResult.Black;
//     CellResult.White;
//     CellResult.Black;
//     CellResult.Black;
//     CellResult.Black;
//     CellResult.White;
//     CellResult.Black;
//     CellResult.White;
//     CellResult.Black;
//     CellResult.Black;
//     CellResult.Black;
//     CellResult.White;
//     CellResult.Black;
//     CellResult.Black;
//     CellResult.Black;
//     CellResult.Black;
//     CellResult.Black;
//     CellResult.Black;
//     CellResult.Black;
//     CellResult.Black;
//     CellResult.Black;
//     CellResult.Black;
//     CellResult.White;
//     CellResult.Black;
//     CellResult.Black;
//     ]
//   
//   let test = [1;3;1;3;10;2]
//   
//   let result = testAxis axis test
//   
//   match result with
//   | Pass -> printf "Y  "
//   | Fail -> printf "N  "
//   | Indeterminate -> printf "?  "

// let printListGrid = fun (arr:CellResult list list) ->
//   use output = new StringWriter()
//   for x in arr do
//     printRow output x
//     fprintfn output ""
//   
//   Console.WriteLine(output) |> ignore
//   arr

[<EntryPoint>]
let main argv =
  
  let origPosition = (Console.CursorLeft, Console.CursorTop)
  
  let start = convertPatternToResult Puzzle.initialGrid
    
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
