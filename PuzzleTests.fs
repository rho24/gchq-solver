module PuzzleTests

open Fuchu
open solver

let tests =
  testList "tests" [
    testCase "trySolveAxis 1" <|
      fun _ -> 
        let axis = [
                CellResult.Unknown;
                CellResult.Unknown;
                CellResult.Unknown;
                ]
             
        let test = [3]

        let expected = [
                CellResult.Black;
                CellResult.Black;
                CellResult.Black;
                ]
        Assert.Equal("answer", expected, trySolveAxis axis test)
    testCase "trySolveAxis 2" <|
      fun _ -> 
        let axis = [
                CellResult.Unknown;
                CellResult.Unknown;
                CellResult.Unknown;
                ]
             
        let test = []

        let expected = [
                CellResult.White;
                CellResult.White;
                CellResult.White;
                ]
        Assert.Equal("answer", expected, trySolveAxis axis test)
    testCase "trySolveAxis 3" <|
      fun _ -> 
        let axis = [
                CellResult.Unknown;
                CellResult.Unknown;
                CellResult.Unknown;
                CellResult.Unknown;
                CellResult.Unknown;
                ]
             
        let test = [3]

        let expected = [
                CellResult.Unknown;
                CellResult.Unknown;
                CellResult.Black;
                CellResult.Unknown;
                CellResult.Unknown;
                ]
        Assert.Equal("answer", expected, trySolveAxis axis test)
    testCase "trySolveAxis 4" <|
      fun _ -> 
        let axis = [
                CellResult.Black;
                CellResult.Unknown;
                CellResult.Unknown;
                CellResult.Unknown;
                CellResult.Unknown;
                ]
             
        let test = [3]

        let expected = [
                CellResult.Black;
                CellResult.Black;
                CellResult.Black;
                CellResult.White;
                CellResult.White;
                ]
        Assert.Equal("answer", expected, trySolveAxis axis test)
      ]
let brokenTests =
  testList "broken tests" [
    testCase "trySolveAxis 5" <|
      fun _ -> 
        let axis = [
                CellResult.Unknown;
                CellResult.Unknown;
                CellResult.Unknown;
                CellResult.Unknown;
                CellResult.Unknown;
                CellResult.Unknown;
                CellResult.Black;
                CellResult.Unknown;
                CellResult.White;
                CellResult.Unknown;
                CellResult.Unknown;
                CellResult.Unknown;
                CellResult.Unknown;
                CellResult.Unknown;
                CellResult.Unknown;
                CellResult.Unknown;
                CellResult.Unknown;
                CellResult.Unknown;
                CellResult.Unknown;
                CellResult.Unknown;
                CellResult.Unknown;
                CellResult.Black;
                CellResult.Unknown;
                CellResult.Unknown;
                CellResult.Unknown;
                ]
             
        let test = [7; 2; 1; 1; 7;]

        let expected = [
                CellResult.Unknown;
                CellResult.Black;
                CellResult.Black;
                CellResult.Black;
                CellResult.Black;
                CellResult.Black;
                CellResult.Black;
                CellResult.Unknown;
                CellResult.White;
                CellResult.Unknown;
                CellResult.Unknown;
                CellResult.Unknown;
                CellResult.Unknown;
                CellResult.Unknown;
                CellResult.Unknown;
                CellResult.Unknown;
                CellResult.Unknown;
                CellResult.Unknown;
                CellResult.Unknown;
                CellResult.Unknown;
                CellResult.Unknown;
                CellResult.Black;
                CellResult.Unknown;
                CellResult.Unknown;
                CellResult.Unknown;
                ]
        Assert.Equal("answer", expected, trySolveAxis axis test)
  ]
