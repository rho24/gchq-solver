module utility

let tap f x = f x

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

let foldi fold first source = 
  source
  |> List.fold (fun (prev, i) c -> (fold i prev c, i + 1)) (first, 0)
  |> fst

let rotate (arr : 'T [,]) = Array2D.init (Array2D.length2 arr) (Array2D.length1 arr) (fun x y -> arr.[y, x])
