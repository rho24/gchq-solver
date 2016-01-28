namespace Sudoku

module Array2D = 
  let toSeq (arr : 'T [,]) = 
    arr
    |> Seq.cast<'T>
