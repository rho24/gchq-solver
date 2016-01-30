namespace Sudoku

type PuzzleSolution = 
  { puzzle : Puzzle
    audit : (string * Puzzle) list }

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
  
  let ruleGuessSimplest solution = 
    let smallestPossible = 
      solution.puzzle
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
    | Some(x, y, Possibles p) -> 
      p |> List.map (fun v -> 
             let newPuzzle = Some(x, y, Value v)
                             |> Puzzle.replaceCell
                             <| solution.puzzle
             match newPuzzle = solution.puzzle with
             | true -> solution
             | false -> 
               { solution with puzzle = newPuzzle
                               audit = ("GuessCell", newPuzzle) :: solution.audit })
  
  let solveCellRule rule solution = 
    solution.puzzle
    |> Puzzle.cells
    |> List.fold (fun s c -> 
         let newPuzzle = 
           c
           |> rule
           |> Puzzle.replaceCell
           <| s.puzzle
         match newPuzzle = s.puzzle with
         | true -> s
         | false -> 
           { solution with puzzle = newPuzzle
                           audit = ("CellReplace", newPuzzle) :: s.audit }) solution
  
  let solveGroupRule rule solution = 
    solution.puzzle
    |> Puzzle.groups
    |> List.fold (fun s c -> 
         let newPuzzle = 
           c
           |> rule
           |> Puzzle.replaceGroup
           <| s.puzzle
         match newPuzzle = s.puzzle with
         | true -> s
         | false -> 
           { solution with puzzle = newPuzzle
                           audit = ("GroupReplace", newPuzzle) :: s.audit }) solution
  
  let rec trySolve guessDepth (solution : PuzzleSolution) : PuzzleSolution option = 
    let simplifiedSolution = 
      { solution with puzzle = solution.puzzle |> simplifyPossibles (solution.puzzle |> Puzzle.completeCells) }
    if simplifiedSolution.puzzle |> Puzzle.isComplete then Some solution
    else if not (simplifiedSolution.puzzle |> Puzzle.isValid) then None
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
             :: p) [ simplifiedSolution ]
        |> List.tryFind (function 
             | l1 :: l2 :: _ -> l1.puzzle <> l2.puzzle
             | _ -> false)
      
      match patternChanges, guessDepth with
      | Some(head :: _), _ -> trySolve guessDepth head
      | _, g when g >= 10 -> Some simplifiedSolution
      | _ -> 
        let guessingRules = [ ruleGuessSimplest ]
        
        let guessedResult = 
          guessingRules
          |> List.collect (fun r -> simplifiedSolution |> r)
          |> List.choose (fun p -> p |> trySolve (guessDepth + 1))
          |> List.tryHead
        guessedResult
  
  let solve puzzle = 
    let solution = 
      { puzzle = puzzle
        audit = [] }
    match trySolve 0 solution with
    | Some solved -> solved
    | None -> solution
