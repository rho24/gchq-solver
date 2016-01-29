namespace Sudoku

module SolverUtils = 
  let rec simplifyPossibles completeCells puzzle = 
    match completeCells with
    | [] -> puzzle
    | (x, y, value) :: rest -> 
      let (Puzzle grid) = puzzle
      grid
      |> Array2D.mapi (fun py px pcell -> 
           if Cell.areRelated (x, y) (px, py) then pcell |> Cell.removeValue value
           else pcell)
      |> Puzzle
      |> simplifyPossibles rest

module Solver = 
  open SolverUtils
  
  let ruleOnePossibleLeft (x, y, cell) = 
    match cell with
    | Possibles([ v ]) -> Some(x, y, Value v)
    | _ -> None
  
  let ruleMatchingPairs (group : CellGroup) = 
    let { cells = cells } = group
    
    let pairs = 
      cells
      |> List.choose (fun c -> 
           match c with
           | Possibles(p) -> Some c
           | _ -> None)
      |> List.groupBy (fun c -> c)
      |> List.filter (function 
           | (Possibles k, v) -> List.length k = 2 && List.length v = 2
           | _ -> false)
      |> List.map (fun (k, _) -> k)
    match pairs with
    | [] -> None
    | pairs -> 
      let newCells = 
        pairs |> List.fold (fun a p -> 
                   a |> List.map (fun c -> 
                          match c <> p, p with
                          | true, (Possibles p) -> Cell.removeValues p c
                          | _ -> c)) cells
      Some { group with cells = newCells }
  
  let solveSecond puzzle = 
    puzzle
    |> Puzzle.groups
    |> List.fold (fun p c -> 
         c
         |> ruleMatchingPairs
         |> Puzzle.replaceGroup
         <| p) puzzle
  
  let rec solve (puzzle : Puzzle) : Puzzle = 
    if puzzle |> Puzzle.isComplete then puzzle
    else 
      let p1 = puzzle |> simplifyPossibles (puzzle |> Puzzle.completeCells)
      
      let p2 = 
        p1
        |> Puzzle.cells
        |> List.fold (fun p c -> 
             c
             |> ruleOnePossibleLeft
             |> Puzzle.replaceCell
             <| p) p1
      match p2 = puzzle with
      | true -> puzzle
      | false -> 
        let p3 = solveSecond p2
        match p3 = puzzle with
        | true -> puzzle
        | false -> solve p3
// try cell based rule
// if no changes try group based
// if no changes try grid based (hooks?)
