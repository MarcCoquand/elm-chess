module Test.Generated.Main2870181909 exposing (main)

import Example
import SquareTest

import Test.Reporter.Reporter exposing (Report(..))
import Console.Text exposing (UseColor(..))
import Test.Runner.Node
import Test

main : Test.Runner.Node.TestProgram
main =
    [     Test.describe "Example" [Example.suite],     Test.describe "SquareTest" [SquareTest.suite] ]
        |> Test.concat
        |> Test.Runner.Node.run { runs = Nothing, report = (ConsoleReport UseColor), seed = 301249264009215, processes = 8, paths = ["/home/marccoquand/Projects/chessElm/tests/Example.elm","/home/marccoquand/Projects/chessElm/tests/SquareTest.elm"]}