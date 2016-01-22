module PuzzleTests

open Fuchu
open solver

let tests = 
  testList "tests" [ testCase "trySolveAxis 1" <| fun _ -> 
                       let axis = [ U; U; U ]
                       let test = [ 3 ]
                       let expected = [ B; B; B ]
                       Assert.Equal("answer", expected, trySolveAxis axis test (getPossibles axis.Length test))
                     testCase "trySolveAxis 2" <| fun _ -> 
                       let axis = [ U; U; U ]
                       let test = []
                       let expected = [ W; W; W ]
                       Assert.Equal("answer", expected, trySolveAxis axis test (getPossibles axis.Length test))
                     testCase "trySolveAxis 3" <| fun _ -> 
                       let axis = [ U; U; U; U; U ]
                       let test = [ 3 ]
                       let expected = [ U; U; B; U; U ]
                       Assert.Equal("answer", expected, trySolveAxis axis test (getPossibles axis.Length test))
                     testCase "trySolveAxis 4" <| fun _ -> 
                       let axis = [ B; U; U; U; U ]
                       let test = [ 3 ]
                       let expected = [ B; B; B; W; W ]
                       Assert.Equal("answer", expected, trySolveAxis axis test (getPossibles axis.Length test)) ]

let brokenTests = 
  testList "broken tests" [ testCase "trySolveAxis 5" <| fun _ -> 
                              let axis = [ U; U; U; U; U; U; B; U; W; U; U; U; U; U; U; U; U; U; U; U; U; B; U; U; U ]
                              let test = [ 7; 2; 1; 1; 7 ]
                              let expected = 
                                [ U; B; B; B; B; B; B; U; W; U; U; U; U; U; U; U; U; U; B; B; B; B; B; U; U ]
                              Assert.Equal("answer", expected, trySolveAxis axis test (getPossibles axis.Length test))
                            testCase "getPossibles 1" <| fun _ -> 
                              let axisLength = 5
                              let test = [ 3 ]
                              
                              let expected = 
                                [ [ B; B; B; W; W ]
                                  [ W; B; B; B; W ]
                                  [ W; W; B; B; B ] ]
                              Assert.Equal("answer", expected, getPossibles axisLength test)
                            testCase "getPossibles 2" <| fun _ -> 
                              let axisLength = 4
                              let test = [ 1; 1 ]
                              
                              let expected = 
                                [ [ B; W; B; W ]
                                  [ B; W; W; B ]
                                  [ W; B; W; B ] ]
                              Assert.Equal("answer", expected, getPossibles axisLength test)
                            testCase "getPossibles 3" <| fun _ -> 
                              let axisLength = 5
                              let test = [ 2; 1 ]
                              
                              let expected = 
                                [ [ B; B; W; B; W ]
                                  [ B; B; W; W; B ]
                                  [ W; B; B; W; B ] ]
                              Assert.Equal("answer", expected, getPossibles axisLength test) ]
