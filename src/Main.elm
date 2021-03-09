module Main exposing (main)

import Levels exposing (..)
import Playground exposing (..)



---- CONSTANTS ----


gameSize : Number
gameSize =
    10


circleSize : Number
circleSize =
    0.5


innerCircleSize : Number
innerCircleSize =
    0.4


connectionWidth : Number
connectionWidth =
    0.1


doubleConnectionWidth : Number
doubleConnectionWidth =
    0.25



---- TYPES ----


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
    , fadeSelection : Maybe Number
    }


initialModel : Model
initialModel =
    { circles =
        Levels.next 7 7
    , selection = None
    , connections = []
    , fadeSelection = Nothing
    }


main =
    game view update initialModel



---- UPDATE ----


update : Computer -> Model -> Model
update computer model =
    let
        mouseX =
            computer.mouse.x
                |> screen_to_game computer.screen

        mouseY =
            computer.mouse.y
                |> screen_to_game computer.screen

        mousePoint =
            model.circles
                |> List.filter (\{ x, y } -> x == mouseX && y == mouseY)
                |> List.head

        updatedModel =
            if computer.mouse.click then
                case mousePoint of
                    Just point ->
                        case model.selection of
                            None ->
                                { model | selection = One point }

                            One firstSelected ->
                                if firstSelected == point then
                                    -- Deselect the first circle if it's clicked again
                                    { model | selection = None }

                                else if canConnect firstSelected point model.circles model.connections then
                                    -- If the second circle can be connected, maybe add/double/delete a connection
                                    -- TODO: and fade the current selection
                                    { model
                                        | selection = Two firstSelected point
                                        , connections = manageConnections model.connections firstSelected point
                                        , fadeSelection = Just 10
                                    }

                                else
                                    -- ... or make the second circle the new first circle
                                    { model | selection = One point }

                            Two firstSelected secondSelected ->
                                if firstSelected == point then
                                    -- Deselect the first circle if it's clicked again and make the second one the first selected
                                    { model | selection = One secondSelected }

                                else if secondSelected == point then
                                    -- Deselect the second circle if it's clicked again
                                    { model | selection = One firstSelected }

                                else
                                    -- Start a new selection
                                    { model | selection = One point }

                    Nothing ->
                        { model | selection = None }

            else
                model
    in
    case updatedModel.fadeSelection of
        Just fadeSelection ->
            if fadeSelection > 0 then
                { updatedModel | fadeSelection = Just <| fadeSelection - 1 }

            else
                { updatedModel | fadeSelection = Nothing, selection = None }

        Nothing ->
            updatedModel



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
            |> List.map (viewCircle model.connections)
        )
    , viewSelection model.selection model.fadeSelection
    , group
        (model.circles
            |> List.map viewNumConnection
        )
    ]


viewCircle : List Connection -> Point -> Shape
viewCircle connections point =
    let
        connectionsFromPoint =
            connections
                |> List.map
                    (\connection ->
                        case connection of
                            Single first second ->
                                if first == point || second == point then
                                    1

                                else
                                    0

                            Double first second ->
                                if first == point || second == point then
                                    2

                                else
                                    0
                    )
                |> List.sum
                |> toFloat

        color =
            if connectionsFromPoint > point.connections then
                red

            else if connectionsFromPoint == point.connections then
                green

            else
                blue
    in
    group
        [ circle color circleSize
        , circle white innerCircleSize
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
            rectangleForPoints blue a b connectionWidth

        Double a b ->
            group
                -- A larger blue rectangle
                [ rectangleForPoints blue a b doubleConnectionWidth

                -- With a thinner white rectangle in the middle to pretend it's a double rectangle
                , rectangleForPoints white a b (doubleConnectionWidth - connectionWidth * 2)
                ]


viewSelection : Selection -> Maybe Number -> Shape
viewSelection selection fadeSelection =
    let
        fadeBy =
            case fadeSelection of
                Just f ->
                    f / 10

                Nothing ->
                    1
    in
    case selection of
        None ->
            -- Inexistent circle
            circle white 0

        One firstSelected ->
            circle yellow innerCircleSize
                |> move firstSelected.x firstSelected.y
                |> fade fadeBy

        Two firstSelected secondSelected ->
            group
                [ circle yellow innerCircleSize
                    |> move firstSelected.x firstSelected.y
                , circle yellow innerCircleSize
                    |> move secondSelected.x secondSelected.y
                ]
                |> fade fadeBy


viewNumConnection : Point -> Shape
viewNumConnection point =
    words black (String.fromFloat point.connections)
        |> move point.x point.y
        |> scale 0.05



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


canConnect : Point -> Point -> List Point -> List Connection -> Bool
canConnect a b circles connections =
    let
        -- Always order the points in the connection the same way so we can compare the connections
        ( first, second ) =
            if a.x < b.x || a.y < b.y then
                ( a, b )

            else
                ( b, a )

        crossesConnections =
            connections
                |> List.map (isCrossing first second)
                |> List.any identity

        crossesCircles =
            circles
                |> List.map (isCrossingCircle first second)
                |> List.any identity
    in
    (isVertical first second || isHorizontal first second) && not crossesCircles && not crossesConnections


isCrossing : Point -> Point -> Connection -> Bool
isCrossing first second connection =
    let
        ( connFirst, connSecond ) =
            pointsFromConnection connection
    in
    ((isHorizontal first second && isVertical connFirst connSecond)
        && (first.x < connFirst.x && second.x > connFirst.x && first.y > connFirst.y && first.y < connSecond.y)
    )
        || (isVertical first second && isHorizontal connFirst connSecond)
        && (first.x > connFirst.x && first.x < connSecond.x && first.y < connFirst.y && second.y > connFirst.y)


isCrossingCircle : Point -> Point -> Point -> Bool
isCrossingCircle first second circle =
    circle
        /= first
        && circle
        /= second
        && ((isHorizontal first second && circle.y == first.y && circle.x > first.x && circle.x < second.x)
                || (isVertical first second && circle.x == first.x && circle.y > first.y && circle.y < second.y)
           )


isHorizontal : Point -> Point -> Bool
isHorizontal p1 p2 =
    p1.y == p2.y


isVertical : Point -> Point -> Bool
isVertical p1 p2 =
    p1.x == p2.x


pointsFromConnection : Connection -> ( Point, Point )
pointsFromConnection connection =
    case connection of
        Single first second ->
            ( first, second )

        Double first second ->
            ( first, second )


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
