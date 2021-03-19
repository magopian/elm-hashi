module Levels exposing (Connection(..), Point, next)

import Array
import Playground exposing (..)


type alias Point =
    { x : Int
    , y : Int
    , connections : Int
    }


type Connection
    = Single Point Point
    | Double Point Point


type Cell
    = Empty
    | Island
    | Horizontal
    | DoubleHorizontal
    | Vertical
    | DoubleVertical


type Bridge
    = SingleHorizontalBridge Int Int
    | DoubleHorizontalBridge Int Int
    | SingleVerticalBridge Int Int
    | DoubleVerticalBridge Int Int
    | BadBridge


type alias Level =
    { width : Int
    , height : Int
    , islands : List Point
    , bridges : List Connection
    }


type alias Cells =
    { width : Int
    , height : Int
    , cells : Array.Array Cell
    }


next : Level
next =
    let
        level : List String
        level =
            [ "       " -- 6
            , "       " -- 13
            , "  O─O  " -- 20
            , "  │ │  " -- 27
            , "  O─O  " -- 34
            , "       " -- 41
            , "       " -- 48
            ]

        -- level =
        --     [ [ 2, 0, 0, 3, 0, 1, 0 ]
        --     , [ 0, 0, 1, 0, 3, 0, 2 ]
        --     , [ 0, 0, 0, 1, 0, 0, 0 ]
        --     , [ 3, 0, 5, 0, 6, 0, 4 ]
        --     , [ 0, 1, 0, 0, 0, 0, 0 ]
        --     , [ 1, 0, 4, 0, 3, 0, 0 ]
        --     , [ 0, 3, 0, 3, 0, 0, 2 ]
        --     ]
        width =
            List.head level
                |> Maybe.withDefault ""
                |> String.length

        height =
            List.length level

        -- Return a flat list of cells
        cells : Cells
        cells =
            level
                |> List.map
                    (\row ->
                        row
                            |> String.toList
                            |> List.map processCell
                    )
                |> List.concat
                |> Array.fromList
                |> Cells width height

        islands : List Point
        islands =
            Array.indexedMap (cellToMaybePoint cells) cells.cells
                |> Array.toList
                |> List.filterMap identity
                |> List.map (toCenterBasedCoord cells)
                |> Debug.log "islands"

        bridges : List Bridge
        bridges =
            List.concatMap (bridgesFromIsland cells) islands
                |> Debug.log "bridges"
    in
    Level cells.width cells.height islands []


processCell : Char -> Cell
processCell char =
    case char of
        'O' ->
            Island

        '─' ->
            Horizontal

        '│' ->
            Vertical

        '═' ->
            DoubleHorizontal

        '║' ->
            DoubleVertical

        _ ->
            Empty


getCell : Cells -> Int -> Maybe Cell
getCell cells index =
    Array.get index cells.cells


indexAboveOf : Cells -> Int -> Maybe Int
indexAboveOf cells index =
    if index < cells.width then
        -- The current cell is on the top "border": there are no cells above it
        Nothing

    else
        Just <| index - cells.width


cellAboveOf : Cells -> Int -> Maybe Cell
cellAboveOf cells index =
    indexAboveOf cells index
        |> Maybe.andThen (\i -> getCell cells i)


indexRightOf : Cells -> Int -> Maybe Int
indexRightOf cells index =
    if remainderBy cells.width (index + 1) == 0 then
        -- The current cell is on the right "border": there are no cells to its right
        Nothing

    else
        Just <| index + 1


cellRightOf : Cells -> Int -> Maybe Cell
cellRightOf cells index =
    indexRightOf cells index
        |> Maybe.andThen (\i -> getCell cells i)


indexBelowOf : Cells -> Int -> Maybe Int
indexBelowOf cells index =
    if index >= cells.width * (cells.height - 1) then
        -- The current cell is on the bottom "border": there are no cells below it
        Nothing

    else
        Just <| index + cells.width


cellBelowOf : Cells -> Int -> Maybe Cell
cellBelowOf cells index =
    indexBelowOf cells index
        |> Maybe.andThen (\i -> getCell cells i)


indexLeftOf : Cells -> Int -> Maybe Int
indexLeftOf cells index =
    if remainderBy cells.width index == 0 then
        -- The current cell is on the left "border": there are no cells to its left
        Nothing

    else
        Just <| index - 1


cellLeftOf : Cells -> Int -> Maybe Cell
cellLeftOf cells index =
    indexLeftOf cells index
        |> Maybe.andThen (\i -> getCell cells i)


xyToIndex : Int -> Int -> Int -> Int
xyToIndex width x y =
    y * width + x


indexToXY : Int -> Int -> ( Int, Int )
indexToXY width index =
    ( index // width, remainderBy width index )


isHorizontal : Maybe Cell -> Bool
isHorizontal cell =
    cell == Just Horizontal || cell == Just DoubleHorizontal


isVertical : Maybe Cell -> Bool
isVertical cell =
    cell == Just Vertical || cell == Just DoubleVertical


isBridge : Maybe Cell -> Bool
isBridge cell =
    isHorizontal cell || isVertical cell


buildBridge : Cells -> Int -> Int -> Bridge
buildBridge cells start index =
    -- `start` is the starting island index, and `index` is the ending island index
    let
        cell =
            getCell cells index

        nextIndex =
            if isHorizontal cell then
                indexRightOf cells index

            else
                indexBelowOf cells index

        nextCell =
            if isHorizontal cell then
                cellRightOf cells index

            else
                cellBelowOf cells index
    in
    case ( nextIndex, isBridge nextCell ) of
        ( Just ni, True ) ->
            buildBridge cells start ni

        ( Just ni, False ) ->
            case cell of
                Just Horizontal ->
                    SingleHorizontalBridge start ni

                Just DoubleHorizontal ->
                    DoubleHorizontalBridge start ni

                Just Vertical ->
                    SingleVerticalBridge start ni

                _ ->
                    DoubleVerticalBridge start ni

        _ ->
            BadBridge


cellToMaybePoint : Cells -> Int -> Cell -> Maybe Point
cellToMaybePoint cells index cell =
    if cell == Island then
        let
            ( x, y ) =
                indexToXY cells.width index

            connections =
                numConnections (cellAboveOf cells index) isVertical
                    + numConnections (cellRightOf cells index) isHorizontal
                    + numConnections (cellBelowOf cells index) isVertical
                    + numConnections (cellLeftOf cells index) isHorizontal
        in
        Just <| Point x y connections

    else
        Nothing


bridgesFromIsland : Cells -> Point -> List Bridge
bridgesFromIsland cells point =
    let
        index =
            xyToIndex cells.width point.x point.y

        rightIndex =
            indexRightOf cells index

        right =
            case rightIndex of
                Just ri ->
                    if isBridge (cellRightOf cells index) then
                        [ buildBridge cells index ri ]

                    else
                        []

                Nothing ->
                    []

        belowIndex =
            indexBelowOf cells index

        below =
            case belowIndex of
                Just bi ->
                    if isBridge (cellBelowOf cells index) then
                        [ buildBridge cells index bi ]

                    else
                        []

                Nothing ->
                    []
    in
    right ++ below


numConnections : Maybe Cell -> (Maybe Cell -> Bool) -> Int
numConnections cell predicate =
    if predicate cell then
        case cell of
            Just Horizontal ->
                1

            Just Vertical ->
                1

            Just DoubleHorizontal ->
                2

            Just DoubleVertical ->
                2

            _ ->
                0

    else
        0


toCenterBasedCoord : Cells -> Point -> Point
toCenterBasedCoord { width, height } { x, y, connections } =
    -- The 0 is in the center of the grid, so x = x - width // 2 and y = - (y - height // 2)
    Point (x - (width // 2)) -(y - (height // 2)) connections
