module solver

open utility
open common

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

let trySolveAxis (axis : CellResult list) (testGroups : int list) (possibles : CellResult list list) = 
  let testAnswer = 
    match testGroups.Length with
    | 0 -> List.replicate axis.Length W
    | _ -> 
      testGroups
      |> List.collect (fun groupLength -> W :: List.replicate groupLength B)
      |> List.tail
  
  let result = 
    match testGroups with
    | [] -> List.replicate axis.Length W
    | _ -> 
      let minimumSolutionWidth = List.length testAnswer
      match axis.Length = minimumSolutionWidth with
      | true -> testAnswer
      | false -> 
        let filteredPossibles = possibles |> List.filter (fun r -> not (List.exists2 cellIncompatible axis r))
        if filteredPossibles.Length = 0 then axis
        else 
          filteredPossibles
          |> List.reduce (List.map2 cellPreferUnknown)
          |> List.map2 cellPreferFirst axis
  
  result

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
