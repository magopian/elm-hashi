module Main exposing (main)

import Playground exposing (..)


gameSize =
    10


type alias Point =
    { x : Number
    , y : Number
    }


type alias Model =
    List Point


initialModel : Model
initialModel =
    [ Point 0 0
    , Point 0 2
    , Point -1 4
    , Point 2 4
    ]

main =
    game view update initialModel



---- UPDATE ----


update: (Computer -> Model -> Model)
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


view: Computer -> Model -> List Shape
view computer model =
    let
        screenSize =
            min computer.screen.width computer.screen.height
    in
    [ group (viewGame model)
        |> scale (game_to_screen computer.screen)
    ]


viewGame : Model -> List Shape
viewGame model =
    [ group
        (model
            |> List.map
                (\point ->
                    viewCircle
                        |> move point.x point.y
                )
        )
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
