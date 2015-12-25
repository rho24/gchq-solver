module solver

open System
open System.IO
open System.Text
open System.Threading

let tap f x =
    f x
    x

type group = int list
type groupsIntermediate = { FinishedGroups : group list; CurrentGroup : group}
type CellResult = Unknown | Black | White
type TestResult = Indeterminate | Pass | Fail

let cellPreferUnknown first second =
  match (first,second) with
  | (Black, Black) -> Black
  | (White, White) -> White
  | _ -> Unknown

let cellPreferFirst first second =
  match (first,second) with
  | (Unknown, _) -> second
  | _ -> first

let cellIncompatible first second =
  match (first, second) with
  | (Black, White) -> true
  | (White, Black) -> true
  | _ -> false


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
  match List.exists (fun x -> x = Unknown) axis with
  | true -> Indeterminate
  | _ ->
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
  
  let testAnswer =
    match testGroups.Length with
    | 0 -> List.replicate axis.Length White
    | _ ->
         testGroups
          |> List.collect (fun groupLength -> CellResult.White :: List.init groupLength (fun _ -> CellResult.Black))
          |> List.tail

//  printfn "trySolve %A %A" axis testGroups
  let result = 
    match testGroups with
    | [] -> List.map (fun r -> CellResult.White) axis
    | _ -> 
      let minimumSolutionWidth = List.length testAnswer
      match axis.Length - minimumSolutionWidth with
      | overlap when overlap = 0 ->
          testAnswer
      | overlap ->
        let possibles =
          [0..overlap]
            |> List.map (fun i -> List.replicate i CellResult.White @ testAnswer @ List.replicate (overlap - i) CellResult.White)
            |> List.filter (fun r -> not (List.exists2 cellIncompatible axis r))
            |> tap (printfn "%A")
        if possibles.Length = 0 then axis else
          possibles
          |> List.reduce (List.map2 cellPreferUnknown)
          |> List.map2 cellPreferFirst axis
      
  result
