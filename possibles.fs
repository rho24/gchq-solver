module possibles

open common

let rec getPossibles axisLength testGroups = 
  let minimulSolutionLength = (List.sum testGroups) + testGroups.Length - 1
  let overlap = axisLength - minimulSolutionLength
  match testGroups, axisLength with
  | [], _ | _, 0 -> [ List.replicate axisLength W ]
  | group :: rest, _ -> 
    let groupPossibles = 
      [ for i in 0..overlap -> 
          let possible = (List.replicate i W) @ (List.replicate group B)
          possible ]
    List.collect (fun x -> x) 
      [ for possible in groupPossibles -> 
          if possible.Length = axisLength then [ possible ]
          else 
            [ for p in getPossibles (axisLength - possible.Length - 1) rest -> possible @ [ W ] @ p ] ]
