module Test.Generated.Main1066159409 exposing (main)

import SquareTest
import MovesTest

import Test.Reporter.Reporter exposing (Report(..))
import Console.Text exposing (UseColor(..))
import Test.Runner.Node
import Test

main : Test.Runner.Node.TestProgram
main =
    [     Test.describe "SquareTest" [SquareTest.suite],     Test.describe "MovesTest" [MovesTest.suite] ]
        |> Test.concat
        |> Test.Runner.Node.run { runs = Nothing, report = (ConsoleReport UseColor), seed = 46988245547993, processes = 8, paths = ["/home/marccoquand/Projects/chessElm/tests/MovesTest.elm","/home/marccoquand/Projects/chessElm/tests/SquareTest.elm"]}