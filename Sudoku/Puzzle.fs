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

type Puzzle = Puzzle of Cell[,]
  
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
    | _ -> Possibles [One; Two; Three; Four; Five; Six; Seven; Eight; Nine]

  let parseList (grid : int list list) =
    let a = array2D grid
    match Array2D.length1 a, Array2D.length2 a with
    | 9, 9 -> a
              |> Array2D.map parseIntToCell
              |> Puzzle
    | _, _ -> failwith "incorrect grid"

  let isComplete (Puzzle grid) = 
    grid
    |> Array2D.toSeq
    |> Seq.forall (fun c -> match c with |Value c -> true | _ -> false)

  let row i (Puzzle grid) =
    grid.[i..i, *] |> Seq.cast<Cell>

  let rows puzzle =
    seq { for i in 0 .. 8 do yield row i puzzle }
    
  let column i (Puzzle grid) =
    grid.[*, i..i] |> Seq.cast<Cell>

  let columns puzzle =
    seq { for i in 0 .. 8 do yield column i puzzle }