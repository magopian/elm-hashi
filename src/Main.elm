module Main exposing (main)

import Playground exposing (..)


gameSize : Number
gameSize =
    10


type alias Point =
    { x : Number
    , y : Number
    }


type Selection
    = None
    | One Point
    | Two Point Point


type Connection
    = Single Point Point
    | Double Point Point


type alias Model =
    { circles : List Point
    , selection : Selection
    , connections : List Connection
    }


initialModel : Model
initialModel =
    { circles =
        [ Point 0 0
        , Point 0 2
        , Point -1 4
        , Point 2 4
        ]
    , selection = None
    , connections = []
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
            case model.selection of
                None ->
                    { model | selection = One mousePoint }

                One firstSelected ->
                    if firstSelected == mousePoint then
                        -- Deselect the first circle if it's clicked again
                        { model | selection = None }

                    else if can_connect firstSelected mousePoint then
                        -- If the second circle can be connected, maybe add/double/delete a connection
                        -- TODO: and fade the current selection
                        { model
                            | selection = Two firstSelected mousePoint
                            , connections = manageConnections model.connections firstSelected mousePoint
                        }

                    else
                        -- ... or make the second circle the new first circle
                        { model | selection = One mousePoint }

                Two firstSelected secondSelected ->
                    if firstSelected == mousePoint then
                        -- Deselect the first circle if it's clicked again and make the second one the first selected
                        { model | selection = One secondSelected }

                    else if secondSelected == mousePoint then
                        -- Deselect the second circle if it's clicked again
                        { model | selection = One firstSelected }

                    else
                        -- Start a new selection
                        { model | selection = One mousePoint }

        else
            { model | selection = None }

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
    [ group
        (model.connections
            |> List.map viewConnection
        )
    , group
        (model.circles
            |> List.map viewCircle
        )
    , viewSelection model.selection
    ]


viewCircle : Point -> Shape
viewCircle point =
    group
        [ circle blue 0.5
        , circle white 0.4
        ]
        |> move point.x point.y


rectangleForPoints : Color -> Point -> Point -> Number -> Shape
rectangleForPoints color a b width =
    -- Rectangles for the connections have an interesting property:
    -- they are either horizontal (the "y" of their points are the same) or vertical (the "x" are the same)
    let
        x =
            -- Make sure we subtract the min to the max of the distance
            max a.x b.x - min a.x b.x

        y =
            max a.y b.y - min a.y b.y
    in
    if x > y then
        -- Horizontal
        rectangle color x width
            -- Move the center of the rectangle over the first point: --O--  O
            |> move a.x a.y
            -- Move the center so the rectangle is touching the circle: O----O
            |> move ((b.x - a.x) / 2) 0

    else
        -- Vertical
        rectangle color width y
            |> move a.x a.y
            |> move 0 ((b.y - a.y) / 2)


viewConnection : Connection -> Shape
viewConnection connection =
    case connection of
        Single a b ->
            rectangleForPoints blue a b 0.1

        Double a b ->
            group
                -- A larger blue rectangle
                [ rectangleForPoints blue a b 0.25

                -- With a thinner white rectangle in the middle to pretend it's a double rectangle
                , rectangleForPoints white a b 0.05
                ]


viewSelection : Selection -> Shape
viewSelection selection =
    case selection of
        None ->
            -- Inexistent circle
            circle white 0

        One firstSelected ->
            circle yellow 0.4
                |> move firstSelected.x firstSelected.y

        Two firstSelected secondSelected ->
            group
                [ circle yellow 0.4
                    |> move firstSelected.x firstSelected.y
                , circle yellow 0.4
                    |> move secondSelected.x secondSelected.y
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


manageConnections : List Connection -> Point -> Point -> List Connection
manageConnections connections a b =
    let
        -- Always order the points in the connection the same way so we can compare the connections
        ( first, second ) =
            if a.x < b.x || a.y < b.y then
                ( a, b )

            else
                ( b, a )

        single =
            Single first second

        double =
            Double first second
    in
    if List.member single connections then
        removeConnection connections single ++ [ double ]

    else if List.member double connections then
        removeConnection connections double

    else
        connections ++ [ single ]


removeConnection : List Connection -> Connection -> List Connection
removeConnection connections connection =
    connections
        |> List.filter (\conn -> conn /= connection)
