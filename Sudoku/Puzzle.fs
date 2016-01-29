namespace Sudoku

type Value = 
  | One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine

type Cell = 
  | Value of Value
  | Possibles of Value list

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Cell = 
  let removeValue value cell = 
    match cell with
    | Value _ -> cell
    | Possibles p -> 
      p
      |> List.filter (fun v -> v <> value)
      |> Possibles
  
  let getSquare (x, y) = (3 * y % 3) + x % 3
  
  let areRelated coords1 coords2 = 
    match coords1, coords2 with
    | (x1, y1), (x2, y2) when x1 = x2 || y1 = y2 -> true
    | _ -> 
      match (getSquare coords1), (getSquare coords2) with
      | s1, s2 when s1 = s2 -> true
      | _ -> false

type Puzzle = 
  | Puzzle of Cell [,]

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Puzzle = 
  let private parseIntToCell i = 
    match i with
    | 1 -> Value One
    | 2 -> Value Two
    | 3 -> Value Three
    | 4 -> Value Four
    | 5 -> Value Five
    | 6 -> Value Six
    | 7 -> Value Seven
    | 8 -> Value Eight
    | 9 -> Value Nine
    | _ -> Possibles [ One; Two; Three; Four; Five; Six; Seven; Eight; Nine ]
  
  let parseList (grid : int list list) = 
    let a = array2D grid
    match Array2D.length1 a, Array2D.length2 a with
    | 9, 9 -> 
      a
      |> Array2D.map parseIntToCell
      |> Puzzle
    | _, _ -> failwith "incorrect grid"
  
  let isComplete (Puzzle grid) = 
    grid
    |> Array2D.toSeq
    |> Seq.forall (fun c -> 
         match c with
         | Value c -> true
         | _ -> false)
  
  let row i (Puzzle grid) = grid.[i..i, *] |> Seq.cast<Cell>
  
  let rows puzzle = 
    seq { 
      for i in 0..8 do
        yield row i puzzle
    }
  
  let column i (Puzzle grid) = grid.[*, i..i] |> Seq.cast<Cell>
  
  let columns puzzle = 
    seq { 
      for i in 0..8 do
        yield column i puzzle
    }
  
  let cells (Puzzle grid) = 
    grid
    |> Array2D.mapi (fun x y cell -> x, y, cell)
    |> Seq.cast<int * int * Cell>
    |> List.ofSeq
  
  let completeCells puzzle = 
    puzzle
    |> cells
    |> List.choose (function 
         | x, y, Value(v) -> Some(x, y, v)
         | _ -> None)
