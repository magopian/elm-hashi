module Main exposing (main)

import Playground exposing (..)


gamewidth =
    10


type alias Model =
    { x : Int
    , y : Int
    }


initModel =
    { x = 0, y = 0 }


main =
    game view update initModel


update computer model =
    model


view computer model =
    let
        screenSize =
            min computer.screen.width computer.screen.height
    in
    [ group (viewGame model)
        |> scale (screenSize / gamewidth)
    ]


viewGame { x, y } =
    [ group
        [ viewCircle |> move 0 0
        , viewCircle |> move 0 2
        , viewCircle |> move -1 4
        , viewCircle |> move 2 4
        ]
    ]


viewCircle =
    group
        [ circle blue 0.5
        , circle white 0.4
        ]
