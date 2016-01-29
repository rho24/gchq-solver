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
      if p2 = puzzle then puzzle
      else solve p2



// try cell based rule
// if no changes try group based
// if no changes try grid based (hooks?)
