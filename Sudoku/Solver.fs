namespace Sudoku

module SolverUtils =

  let rec simplify (completeCells : (int*int*Value) list) puzzle =
    match completeCells with
    | [] -> puzzle
    | (x,y,value) :: rest ->
      let (Puzzle grid) = puzzle
      let p2 = 
        grid
        |> Array2D.mapi (fun px py pcell -> if Cell.areRelated (x,y) (px,py) then 
                                              pcell |> Cell.removeValue value 
                                            else pcell)
        |> Puzzle
      simplify rest p2

module Solver =
  open SolverUtils
  let solve (puzzle:Puzzle) : Puzzle =
    puzzle
    |> simplify (puzzle |> Puzzle.completeCells)