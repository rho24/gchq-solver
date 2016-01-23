module possibles

open common

let rec getPossibles axisLength testGroups = 
  let minimulSolutionLength = (List.sum testGroups) + testGroups.Length - 1
  let overlap = axisLength - minimulSolutionLength
  match testGroups with
  | [] -> [ List.replicate axisLength W ]
  | group :: rest -> 
    let groupPossibles = 
      [ for i in 0..overlap -> 
          let possible = (List.replicate i W) @ (List.replicate group B)
          possible ]
    groupPossibles
    |> List.map (fun possible -> 
         match possible.Length = axisLength with
         | true -> [ possible ]
         | false -> getPossibles (axisLength - possible.Length - 1) rest |> List.map (fun c -> possible @ (W :: c)))
    |> List.concat
