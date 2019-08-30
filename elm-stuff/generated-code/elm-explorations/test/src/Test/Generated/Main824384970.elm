module Test.Generated.Main824384970 exposing (main)

import PositionTest
import SquareTest

import Test.Reporter.Reporter exposing (Report(..))
import Console.Text exposing (UseColor(..))
import Test.Runner.Node
import Test

main : Test.Runner.Node.TestProgram
main =
    [     Test.describe "PositionTest" [PositionTest.suite],     Test.describe "SquareTest" [SquareTest.suite] ]
        |> Test.concat
        |> Test.Runner.Node.run { runs = Nothing, report = (ConsoleReport UseColor), seed = 319722144141407, processes = 8, paths = ["/home/marccoquand/Projects/chessElm/tests/PositionTest.elm","/home/marccoquand/Projects/chessElm/tests/SquareTest.elm"]}