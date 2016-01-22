module solver

open System
open System.IO
open System.Text
open System.Threading

let tap f x = 
  f x
  x

type group = int list

type groupsIntermediate = 
  { FinishedGroups : group list
    CurrentGroup : group }

type CellResult = 
  | U
  | B
  | W

type TestResult = 
  | Indeterminate
  | Pass
  | Fail

let cellPreferUnknown first second = 
  match (first, second) with
  | (B, B) -> B
  | (W, W) -> W
  | _ -> U

let cellPreferFirst first second = 
  match (first, second) with
  | (U, _) -> second
  | _ -> first

let cellIncompatible first second = 
  match (first, second) with
  | (B, W) -> true
  | (W, B) -> true
  | _ -> false

let row i (arr : 'T [,]) = 
  arr.[i..i, *]
  |> Seq.cast<'T>
  |> Seq.toList

let column i (arr : 'T [,]) = 
  arr.[*, i..i]
  |> Seq.cast<'T>
  |> Seq.toList

let rows (arr : 'T [,]) = [ 0..Array2D.length1 arr - 1 ] |> List.map (fun i -> row i arr)
let columns (arr : 'T [,]) = [ 0..Array2D.length2 arr - 1 ] |> List.map (fun i -> column i arr)

let public foldi fold first source = 
  source
  |> List.fold (fun (prev, i) c -> (fold i prev c, i + 1)) (first, 0)
  |> fst

let rotate (arr : 'T [,]) = Array2D.init (Array2D.length2 arr) (Array2D.length1 arr) (fun x y -> arr.[y, x])

let convertPatternToResult grid : CellResult [,] = 
  Array2D.map (fun v -> 
    match v with
    | 1 -> B
    | _ -> U) grid

let printRow output row = 
  for i in row do
    match i with
    | U -> fprintf output "??"
    | B -> fprintf output "  "
    | W -> fprintf output "\u2588\u2588"
  fprintf output "\u2588\u2588"

let getGroups (axis) = 
  foldi (fun index acc elem -> 
    match elem with
    | W -> 
      match List.length acc.CurrentGroup with
      | 0 -> acc
      | _ -> 
        { acc with FinishedGroups = acc.CurrentGroup :: acc.FinishedGroups
                   CurrentGroup = [] }
    | _ -> { acc with CurrentGroup = index :: acc.CurrentGroup }) { FinishedGroups = []
                                                                    CurrentGroup = [] } axis
  |> fun acc -> 
    match List.length acc.CurrentGroup with
    | 0 -> acc.FinishedGroups
    | _ -> acc.CurrentGroup :: acc.FinishedGroups
    |> List.map (fun l -> List.rev l)
    |> List.rev

let testAxis (axis) (test : int list) = 
  match List.exists (fun x -> x = U) axis with
  | true -> Indeterminate
  | _ -> 
    let axisGroups = getGroups axis
    let axisGroupCount = List.length axisGroups
    let testCount = List.length test
    match (axisGroupCount, testCount) with
    | (a, b) when a > b -> Fail
    | (a, b) when a = b -> 
      match List.forall2 (fun axisGroup testGroup -> (List.length axisGroup) = testGroup) axisGroups test with
      | true -> Pass
      | false -> Fail
    | _ -> Indeterminate

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
  fprintfn output ""
  fprintfn output ""
  fprintf output "   "
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
    printRow output row
    fprintfn output ""
  fprintf output "   "
  for i in 1..27 do
    fprintf output "\u2588\u2588"
  output.ToString()

let rnd = Random()

let addRandomGuess grid = 
  let x = rnd.Next(0, 25)
  let y = rnd.Next(0, 25)
  
  let z = 
    if rnd.Next(0, 2) = 0 then B
    else W
  Array2D.mapi (fun i j v -> 
    if i = x && j = y then z
    else v) grid

let rec getPossibles axisLength testGroups = 
  let minimulSolutionLength = (List.sum testGroups) + testGroups.Length - 1
  let overlap = axisLength - minimulSolutionLength
  match testGroups, axisLength with
  | [], _ | _, 0 -> [ List.replicate axisLength W ]
  | group :: rest, _ -> 
    let groupPossibles = 
      [ for i in 0..overlap -> 
          let possible = (List.replicate i W) @ (List.replicate group B)
          possible ]
    List.collect (fun x -> x) 
      [ for possible in groupPossibles -> 
          if possible.Length = axisLength then [ possible ]
          else 
            [ for p in getPossibles (axisLength - possible.Length - 1) rest -> possible @ (List.replicate 1 W) @ p ] ]

let trySolveAxis axis (testGroups : int list) (possibles : CellResult list list) = 
  let axisGroups = getGroups axis
  let axisGroupCount = List.length axisGroups
  let testCount = List.length testGroups
  
  let testAnswer = 
    match testGroups.Length with
    | 0 -> List.replicate axis.Length W
    | _ -> 
      testGroups
      |> List.collect (fun groupLength -> CellResult.W :: List.init groupLength (fun _ -> CellResult.B))
      |> List.tail
  
  let result = 
    match testGroups with
    | [] -> List.map (fun r -> CellResult.W) axis
    | _ -> 
      let minimumSolutionWidth = List.length testAnswer
      match axis.Length - minimumSolutionWidth with
      | overlap when overlap = 0 -> testAnswer
      | overlap -> 
        let filteredPossibles = possibles |> List.filter (fun r -> not (List.exists2 cellIncompatible axis r))
        if filteredPossibles.Length = 0 then axis
        else 
          filteredPossibles
          |> List.reduce (List.map2 cellPreferUnknown)
          |> List.map2 cellPreferFirst axis
  
  result
