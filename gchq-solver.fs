module gchq

open System

type group = int list
type groupsIntermediate = { FinishedGroups : group list; CurrentGroup : group}
type CellResult = Unknown | Black | White
type TestResult = Indeterminate | Pass | Fail

let row i (arr: 'T[,]) = arr.[i..i, *] |> Seq.cast<'T> |> Seq.toArray
let column i (arr: 'T[,]) = arr.[*, i..i] |> Seq.cast<'T> |> Seq.toArray

let rows (arr: 'T[,]) = 
  [ 0.. Array2D.length1 arr - 1 ] |> List.map (fun i -> row i arr)
  
let columns (arr: 'T[,]) = 
  [ 0.. Array2D.length2 arr - 1 ] |> List.map (fun i -> column i arr)

let public foldi fold first source  =
        source 
        |> Array.fold(fun (prev,i) c -> (fold i prev c,i + 1)) (first,0)
        |> fst

let convertPatternToResult grid :CellResult[,] =
  Array2D.map (fun v ->
    match v with
    | 1 -> Black
    | _ -> Unknown
    ) grid

let printRow (row) =
  for i in row do
    match i with
    | Unknown -> printf "?"
    | Black -> printf "1"
    | White -> printf "0"

let printGrid grid =
  let maxY = (Array2D.length1 grid) - 1
  
  for i in 0 .. maxY do
    let row = row i grid
    printRow row
    printfn ""
    
let printGridRotated grid =
  let maxX = (Array2D.length2 grid) - 1
  
  for i in 0 .. maxX do
    let row = column i grid
    printRow row
    printfn ""

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
      

let testAxis (axis) (test: int list) =
  let axisGroups = getGroups axis
  let axisGroupCount = List.length axisGroups
  let testCount = List.length test
  
  match (axisGroupCount, testCount) with
  | (a,b) when a > b -> Fail
  | (a,b) when a = b -> 
    match List.forall2 (fun axisGroup testGroup -> (List.length axisGroup) = testGroup) axisGroups test with
    | true -> Pass
    | false -> Fail
    
  | _ -> Indeterminate

let printResultGrid (grid) (rowTests : int list list) (columnTests : int list list) =
  let maxY = (Array2D.length1 grid) - 1

  let rowResults = List.map2 testAxis (rows grid) rowTests
  let columnResults = List.map2 testAxis (columns grid) columnTests
  
  printf "   "
  for result in columnResults do
    match result with
    | Pass -> printf "Y"
    | Fail -> printf "N"
    | Indeterminate -> printf "?"
  printfn ""
  printfn ""
  
  for i in 0 .. maxY do
    let row = row i grid
    
    let result = rowResults.[i]
    
    match result with
    | Pass -> printf "Y  "
    | Fail -> printf "N  "
    | Indeterminate -> printf "?  "
    printRow row
    printfn ""
  printfn ""

let rnd = Random()
let addRandomGuess grid =
  let x = rnd.Next(0, 25)
  let y = rnd.Next(0, 25)
  let z = if rnd.Next(0, 2) = 0 then Black else White
  Array2D.mapi (fun i j v -> if i = x && j = y then z else v) grid
    
[<EntryPoint>]
let main argv =
  // printGrid Puzzle.initialGrid
  let start = convertPatternToResult Puzzle.initialGrid
  let guess =   
    [1..100] |> List.fold (fun acc e -> addRandomGuess acc) (convertPatternToResult Puzzle.initialGrid)
  
  printResultGrid start Puzzle.rowTests Puzzle.colTests
  
  printResultGrid guess Puzzle.rowTests Puzzle.colTests
  0 // return an integer exit code
