module gchq

open System

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

let printRow (row:int[]) =
  for i in row do
    printf "%i" i

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

    
type group = int list
type groupsIntermediate = { FinishedGroups : group list; CurrentGroup : group}
type AxisResult = Unknown | Pass | Fail 

let getGroups (axis: int[]) =
  foldi (fun index acc elem -> 
    match elem with
    | 0 -> match List.length acc.CurrentGroup with
            | 0 -> acc
            | _ -> {acc with FinishedGroups = acc.CurrentGroup :: acc.FinishedGroups; CurrentGroup = []}
    | _ -> {acc with CurrentGroup = index :: acc.CurrentGroup}
    ) {FinishedGroups = []; CurrentGroup = []} axis
      |> fun acc -> match List.length acc.CurrentGroup with
                      | 0 -> acc.FinishedGroups
                      | _ -> acc.CurrentGroup :: acc.FinishedGroups
      

let testAxis (axis: int[]) (test: int list) =
  let axisGroups = getGroups axis
  let axisGroupCount = List.length axisGroups
  let testCount = List.length test
  
  match (axisGroupCount, testCount) with
  | (a,b) when a > b -> Fail
  | (a,b) when a = b -> 
    match List.forall2 (fun axisGroup testGroup -> (List.length axisGroup) = testGroup) axisGroups test with
    | true -> Pass
    | false -> Fail
    
  | _ -> Unknown

let printResultGrid grid (rowTests : int list list) (columnTests : int list list) =
  let maxY = (Array2D.length1 grid) - 1
  
  let rowResults = List.map2 testAxis (rows grid) rowTests
  let columnResults = List.map2 testAxis (columns grid) columnTests
  
  printf "   "
  for result in columnResults do
    match result with
    | Pass -> printf "Y"
    | Fail -> printf "N"
    | Unknown -> printf "?"
  printfn ""
  printfn ""
  
  for i in 0 .. maxY do
    let row = row i grid
    
    let result = rowResults.[i]
    
    match result with
    | Pass -> printf "Y  "
    | Fail -> printf "N  "
    | Unknown -> printf "?  "
    printRow row
    printfn ""
    
[<EntryPoint>]
let main argv =
  // printGrid Puzzle.initialGrid
  
  printResultGrid Puzzle.initialGrid Puzzle.rowTests Puzzle.colTests
  0 // return an integer exit code
