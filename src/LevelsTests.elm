module LevelsTests exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Levels exposing (..)
import Test exposing (..)


miniLevel : List String
miniLevel =
    [ "O─O" -- 2
    , "│ │" -- 5
    , "O─O" -- 8
    ]


suite : Test
suite =
    describe "The level parser"
        [ test "correctly parses a minimal level" <|
            \_ ->
                parseLevel miniLevel
                    |> Expect.equal
                        (Level 3
                            3
                            [ Point -1 1 2
                            , Point 1 1 2
                            , Point -1 -1 2
                            , Point 1 -1 2
                            ]
                            []
                        )
        ]
