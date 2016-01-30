#I @"C:\Users\rich\Documents\GitHub\gchq-solver\Sudoku"

let (|>) value func = 
  let result = func value
  result

#load "Scripts/load-project-debug.fsx"


open Sudoku

let valueToInt v = 
  match v with
  | One -> 1
  | Two -> 2
  | Three -> 3
  | Four -> 4
  | Five -> 5
  | Six -> 6
  | Seven -> 7
  | Eight -> 8
  | Nine -> 9

let maybePrintNumber = 
  fun n -> 
    match n with
    | Some i -> sprintf "%2i" i
    | None -> "  "

let hasInt i p = 
  match p |> List.contains i with
  | true -> Some i
  | false -> None

let printPossibles = 
  fun p -> 
    sprintf "%s%s%s\n%s%s%s\n%s%s%s\n" <| maybePrintNumber (hasInt 1 p) <| maybePrintNumber (hasInt 2 p) 
    <| maybePrintNumber (hasInt 3 p) <| maybePrintNumber (hasInt 4 p) <| maybePrintNumber (hasInt 5 p) 
    <| maybePrintNumber (hasInt 6 p) <| maybePrintNumber (hasInt 7 p) <| maybePrintNumber (hasInt 8 p) 
    <| maybePrintNumber (hasInt 9 p)

let print (Puzzle grid) =
    let sb = new System.Text.StringBuilder()
    Printf.bprintf sb "\n"
    for y in 0..8 do
      let c = 
        [ for x in 0..8 do
            yield match grid.[y, x] with
                  | Possibles p -> 
                    p
                    |> List.map valueToInt
                    |> printPossibles
                  | Value v -> 
                    v
                    |> valueToInt
                    |> sprintf "######\n##%2i##\n######\n" ]
      
      let d = 
        c
        |> List.map (fun s -> s.Split [| '\n' |])
        |> List.map (fun ls -> (ls.[0], ls.[1], ls.[2]))
        |> List.unzip3
        |> fun (l1, l2, l3) -> [ l1; l2; l3 ]
      
      d
      |> List.map (fun l -> List.fold (fun a s -> a + "  " + s) "" l)
      |> List.iter (fun l -> Printf.bprintf sb "%s\n" l)
      Printf.bprintf sb "\n"
    sb.ToString()

fsi.AddPrinter(print)

let pEmpty = 
  Puzzle.parseList [ [ 0; 0; 0; 0; 0; 0; 0; 0; 0 ]
                     [ 0; 0; 0; 0; 0; 0; 0; 0; 0 ]
                     [ 0; 0; 0; 0; 0; 0; 0; 0; 0 ]
                     [ 0; 0; 0; 0; 0; 0; 0; 0; 0 ]
                     [ 0; 0; 0; 0; 0; 0; 0; 0; 0 ]
                     [ 0; 0; 0; 0; 0; 0; 0; 0; 0 ]
                     [ 0; 0; 0; 0; 0; 0; 0; 0; 0 ]
                     [ 0; 0; 0; 0; 0; 0; 0; 0; 0 ]
                     [ 0; 0; 0; 0; 0; 0; 0; 0; 0 ] ]

let pEasy = 
  Puzzle.parseList [ [ 5; 3; 0; 0; 7; 0; 0; 0; 0 ]
                     [ 6; 0; 0; 1; 9; 5; 0; 0; 0 ]
                     [ 0; 9; 8; 0; 0; 0; 0; 6; 0 ]
                     [ 8; 0; 0; 0; 6; 0; 0; 0; 3 ]
                     [ 4; 0; 0; 8; 0; 3; 0; 0; 1 ]
                     [ 7; 0; 0; 0; 2; 0; 0; 0; 6 ]
                     [ 0; 6; 0; 0; 0; 0; 2; 8; 0 ]
                     [ 0; 0; 0; 4; 1; 9; 0; 0; 5 ]
                     [ 0; 0; 0; 0; 8; 0; 0; 7; 9 ] ]
                     
let pMedium = 
  Puzzle.parseList [ [ 7; 0; 0; 0; 0; 0; 0; 0; 0 ]
                     [ 0; 0; 1; 0; 7; 0; 0; 0; 8 ]
                     [ 0; 8; 6; 9; 0; 2; 0; 7; 0 ]
                     [ 0; 0; 2; 3; 0; 0; 0; 9; 0 ]
                     [ 6; 9; 0; 0; 0; 0; 0; 8; 5 ]
                     [ 0; 3; 0; 0; 0; 6; 2; 0; 0 ]
                     [ 0; 6; 0; 2; 0; 5; 4; 3; 0 ]
                     [ 9; 0; 0; 0; 8; 0; 7; 0; 0 ]
                     [ 0; 0; 0; 0; 0; 0; 0; 0; 6 ] ]

let pHard = 
  Puzzle.parseList [ [ 6; 0; 0; 2; 0; 0; 0; 4; 0 ]
                     [ 3; 0; 0; 0; 0; 0; 0; 0; 8 ]
                     [ 0; 0; 0; 1; 0; 0; 6; 5; 9 ]
                     [ 4; 0; 0; 3; 0; 0; 0; 0; 7 ]
                     [ 0; 1; 3; 0; 9; 0; 0; 0; 0 ]
                     [ 0; 7; 0; 0; 0; 0; 5; 0; 0 ]
                     [ 2; 0; 0; 0; 1; 0; 0; 0; 0 ]
                     [ 0; 3; 0; 0; 0; 4; 0; 0; 0 ]
                     [ 0; 5; 0; 6; 7; 0; 0; 0; 0 ] ]
                     
let easySolved = pEasy |> Solver.solve

let mediumSolved = pMedium |> Solver.solve

let hardSolved = pHard |> Solver.solve

let easyS = easySolved.puzzle
let easyValid = easySolved.puzzle |> Puzzle.isValid
let easyMoves = easySolved.audit |> List.length
let easyGuesses = easySolved.audit |> List.filter (fun (s,_) -> s = "GuessCell") |> List.length

let mediumS = mediumSolved.puzzle
let mediumValid = mediumSolved.puzzle |> Puzzle.isValid
let mediumMoves = mediumSolved.audit |> List.length
let mediumGuesses = mediumSolved.audit |> List.filter (fun (s,_) -> s = "GuessCell") |> List.length

let hardS = hardSolved.puzzle
let hardValid = hardSolved.puzzle |> Puzzle.isValid
let hardMoves = hardSolved.audit |> List.length
let hardGuesses = hardSolved.audit |> List.filter (fun (s,_) -> s = "GuessCell") |> List.length