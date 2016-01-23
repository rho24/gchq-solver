module common

open utility

type CellResult = 
  | U
  | B
  | W
  
type TestResult = 
  | Indeterminate
  | Pass
  | Fail

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

type group = int list

type groupsIntermediate = 
  { FinishedGroups : group list
    CurrentGroup : group }

let getGroups (axis : CellResult list) = 
  axis
  |> foldi (fun index acc elem -> 
       match elem with
       | W -> 
         match List.length acc.CurrentGroup with
         | 0 -> acc
         | _ -> 
           { acc with FinishedGroups = acc.CurrentGroup :: acc.FinishedGroups
                      CurrentGroup = [] }
       | _ -> { acc with CurrentGroup = index :: acc.CurrentGroup }) { FinishedGroups = []
                                                                       CurrentGroup = [] }
  |> fun acc -> 
    match List.length acc.CurrentGroup with
    | 0 -> acc.FinishedGroups
    | _ -> acc.CurrentGroup :: acc.FinishedGroups
    |> List.map (fun l -> List.rev l)
    |> List.rev

let convertPatternToResult grid : CellResult [,] = 
  Array2D.map (fun v -> 
    match v with
    | 1 -> B
    | _ -> U) grid
