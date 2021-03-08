module Levels exposing (Point, next)

import Playground exposing (..)


type alias Point =
    { x : Number
    , y : Number
    , connections : Number
    }


next : Int -> Int -> List Point
next width height =
    let
        points : List (List Number)
        points =
            [ [ 2, 0, 0, 3, 0, 1, 0 ]
            , [ 0, 0, 1, 0, 3, 0, 2 ]
            , [ 0, 0, 0, 1, 0, 0, 0 ]
            , [ 3, 0, 5, 0, 6, 0, 4 ]
            , [ 0, 1, 0, 0, 0, 0, 0 ]
            , [ 1, 0, 4, 0, 3, 0, 0 ]
            , [ 0, 3, 0, 3, 0, 0, 2 ]
            ]
    in
    points
        |> List.indexedMap
            (\j row ->
                row
                    |> List.indexedMap
                        (\i numConnections -> coordinatesToMaybePoint i j numConnections)
            )
        |> List.concat
        |> List.filterMap identity
        |> List.map (toCenterBasedCoord width height)


coordinatesToMaybePoint : Int -> Int -> Number -> Maybe Point
coordinatesToMaybePoint x y numConnections =
    if numConnections > 0 then
        Just <| Point (toFloat x) (toFloat y) numConnections

    else
        Nothing


toCenterBasedCoord : Int -> Int -> Point -> Point
toCenterBasedCoord width height { x, y, connections } =
    -- The 0 is in the center of the grid, so x = x - width // 2 and y = - (y - height // 2)
    Point (x - toFloat (width // 2)) -(y - toFloat (height // 2)) connections
