module Main exposing (main)

import Playground exposing (..)


gameSize : Number
gameSize =
    10


type alias Point =
    { x : Number
    , y : Number
    }


type Selected
    = None
    | One Point
    | Two Point Point


type alias Model =
    { circles : List Point
    , selectedCircle : Selected
    }


initialModel : Model
initialModel =
    { circles =
        [ Point 0 0
        , Point 0 2
        , Point -1 4
        , Point 2 4
        ]
    , selectedCircle = None
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
            case model.selectedCircle of
                None ->
                    { model | selectedCircle = One mousePoint }

                One firstSelected ->
                    if firstSelected == mousePoint then
                        -- Deselect the first circle if it's clicked again
                        { model | selectedCircle = None }

                    else if can_connect firstSelected mousePoint then
                        -- If the second circle can be connected, add it to the selection ...
                        { model | selectedCircle = Two firstSelected mousePoint }

                    else
                        -- ... or make the second circle the new first circle
                        { model | selectedCircle = One mousePoint }

                Two firstSelected secondSelected ->
                    if firstSelected == mousePoint then
                        -- Deselect the first circle if it's clicked again and make the second one the first selected
                        { model | selectedCircle = One secondSelected }

                    else if secondSelected == mousePoint then
                        -- Deselect the second circle if it's clicked again
                        { model | selectedCircle = One firstSelected }

                    else
                        -- Start a new selection
                        { model | selectedCircle = One mousePoint }

        else
            { model | selectedCircle = None }

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
            case model.selectedCircle of
                None ->
                    False

                One firstSelected ->
                    firstSelected == point

                Two firstSelected secondSelected ->
                    firstSelected == point || secondSelected == point
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


can_connect : Point -> Point -> Bool
can_connect first second =
    first.x == second.x || first.y == second.y
