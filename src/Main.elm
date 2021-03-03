module Main exposing (main)

import Playground exposing (..)


gameSize : Number
gameSize =
    10


type alias Point =
    { x : Number
    , y : Number
    }


type alias Model =
    { circles : List Point
    , selectedCircle : Maybe Point
    }


initialModel : Model
initialModel =
    { circles =
        [ Point 0 0
        , Point 0 2
        , Point -1 4
        , Point 2 4
        ]
    , selectedCircle = Nothing
    }


main =
    game view update initialModel



---- UPDATE ----


update : Computer -> Model -> Model
update computer model =
    let
        _ =
            computer.mouse.x
                |> screen_to_game computer.screen
                |> Debug.log "mouse"

        mouseX =
            computer.mouse.x
                |> screen_to_game computer.screen

        mouseY =
            computer.mouse.y
                |> screen_to_game computer.screen

        mousePoint =
            Point mouseX mouseY
    in
    if computer.mouse.click then
        if List.member mousePoint model.circles then
            { model | selectedCircle = Just mousePoint }

        else
            { model | selectedCircle = Nothing }

    else
        model



---- VIEW ----


view : Computer -> Model -> List Shape
view computer model =
    [ group (viewGame model)
        |> scale (game_to_screen computer.screen)
    ]


viewGame : Model -> List Shape
viewGame model =
    let
        isCircleSelected : Point -> Bool
        isCircleSelected point =
            model.selectedCircle
                |> Maybe.map (\selected -> selected == point)
                |> Maybe.withDefault False
    in
    [ group
        (model.circles
            |> List.map
                (\point ->
                    viewCircle (isCircleSelected point)
                        |> move point.x point.y
                )
        )
    ]


viewCircle : Bool -> Shape
viewCircle selected =
    let
        color =
            if selected then
                yellow

            else
                white
    in
    group
        [ circle blue 0.5
        , circle color 0.4
        ]



---- HELPERS ----


screen_to_game : Screen -> Number -> Number
screen_to_game screen coord =
    let
        screenSize =
            min screen.width screen.height
    in
    coord
        * (gameSize / screenSize)
        |> round
        |> toFloat


game_to_screen : Screen -> Number
game_to_screen screen =
    let
        screenSize =
            min screen.width screen.height
    in
    screenSize / gameSize
