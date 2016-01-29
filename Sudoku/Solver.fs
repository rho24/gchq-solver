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
           | (Possibles k, v) -> 
             let possiblesLength = List.length k
             let cellsLength = List.length v
             possiblesLength > 0 && possiblesLength < 9 && possiblesLength = cellsLength
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
  
  let ruleGuessSimplest puzzle = 
    let smallestPossible = 
      puzzle
      |> Puzzle.cells
      |> List.choose (fun c -> 
           match c with
           | _, _, Possibles(p) when List.length p <= 2 -> Some c
           | _ -> None)
      |> List.sortBy (function 
           | _, _, (Possibles p) -> List.length p
           | _ -> 0)
      |> List.tryHead
    match smallestPossible with
    | None | Some(_, _, Value _) -> []
    | Some(x, y, Possibles p) -> p |> List.map (fun v -> Some(x, y, Value v)
                                                         |> Puzzle.replaceCell
                                                         <| puzzle)
  
  let solveCellRule rule puzzle = 
    puzzle
    |> Puzzle.cells
    |> List.fold (fun p c -> 
         c
         |> rule
         |> Puzzle.replaceCell
         <| p) puzzle
  
  let solveGroupRule rule puzzle = 
    puzzle
    |> Puzzle.groups
    |> List.fold (fun p c -> 
         c
         |> rule
         |> Puzzle.replaceGroup
         <| p) puzzle
  
  let rec trySolve guesses (puzzle : Puzzle) : Puzzle option = 
    let simplifiedPuzzle = puzzle |> simplifyPossibles (puzzle |> Puzzle.completeCells)
    if simplifiedPuzzle |> Puzzle.isComplete then Some puzzle
    else if not (simplifiedPuzzle |> Puzzle.isValid) then None
    else 
      let rules = 
        [ solveCellRule ruleOnePossibleLeft
          solveGroupRule ruleMatchingPairs ]
      
      let patternChanges = 
        rules
        |> List.scan (fun p r -> 
             (p
              |> List.head
              |> r)
             :: p) [ simplifiedPuzzle ]
        |> List.tryFind (function 
             | l1 :: l2 :: _ -> l1 <> l2
             | _ -> false)
      
      match patternChanges, guesses with
      | Some(head :: _), _ -> trySolve guesses head
      | _, g when g >= 10 -> Some simplifiedPuzzle
      | _ -> 
        let guessingRules = [ ruleGuessSimplest ]
        
        let guessedResult = 
          guessingRules
          |> List.collect (fun r -> simplifiedPuzzle |> r)
          |> List.choose (fun p -> p |> trySolve (guesses + 1))
          |> List.tryHead
        guessedResult
  
  let solve puzzle = 
    match trySolve 0 puzzle with
    | Some solved -> solved
    | None -> puzzle
