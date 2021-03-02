module Main exposing (main)

import Playground exposing (..)


gameSize =
    10


type alias Model =
    { x : Int
    , y : Int
    }


initModel =
    { x = 0, y = 0 }


main =
    game view update initModel



---- UPDATE ----


update computer model =
    let
        screenSize =
            min computer.screen.width computer.screen.height

        _ =
            computer.mouse.x
                |> screen_to_game computer.screen
                |> Debug.log "mouse"
    in
    model



---- VIEW ----


view computer model =
    let
        screenSize =
            min computer.screen.width computer.screen.height
    in
    [ group (viewGame model)
        |> scale (game_to_screen computer.screen)
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



---- HELPERS ----


screen_to_game screen coord =
    let
        screenSize =
            min screen.width screen.height
    in
    coord
        * (gameSize / screenSize)
        |> round


game_to_screen screen =
    let
        screenSize =
            min screen.width screen.height
    in
    screenSize / gameSize
