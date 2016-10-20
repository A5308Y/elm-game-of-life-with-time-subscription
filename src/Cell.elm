module Cell exposing (..)

import Set exposing (Set)
import CellInit exposing (cells, altCells)
import SetExtras exposing (setFilterMap, setConcatMap)


type alias Position =
    ( Int, Int )



-- Cell as position with neighbourCount?


type alias Model =
    { cells : Set Position }


initModel : Model
initModel =
    { cells = Set.fromList CellInit.altCells }


updateModel : Model -> Model
updateModel model =
    { cells = updatePositions model.cells }


updatePositions : Set Position -> Set Position
updatePositions cells =
    positionsToCheck cells
        |> setFilterMap (\position -> (updatePosition cells position))


positionsToCheck : Set Position -> Set Position
positionsToCheck cells =
    setConcatMap possibleNeighbours cells
        |> Set.union cells


updatePosition : Set Position -> Position -> Maybe Position
updatePosition cells position =
    let
        neighbourCount =
            Set.size (neighbours cells position)
    in
        if neighbourCount < 2 then
            Nothing
        else if neighbourCount == 3 then
            Just position
        else if neighbourCount == 2 && isCellAt cells position then
            Just position
        else
            Nothing


neighbours : Set Position -> Position -> Set Position
neighbours cells position =
    position
        |> possibleNeighbours
        |> Set.filter (isCellAt cells)


possibleNeighbours : Position -> Set Position
possibleNeighbours position =
    Set.fromList
        [ position `addPosition` ( -1, 1 )
        , position `addPosition` ( -1, 0 )
        , position `addPosition` ( -1, -1 )
        , position `addPosition` ( 0, 1 )
        , position `addPosition` ( 0, -1 )
        , position `addPosition` ( 1, 1 )
        , position `addPosition` ( 1, 0 )
        , position `addPosition` ( 1, -1 )
        ]


addPosition : Position -> Position -> Position
addPosition leftPosition rightPosition =
    ( fst leftPosition + fst rightPosition, snd leftPosition + snd rightPosition )


isCellAt : Set Position -> Position -> Bool
isCellAt cells position =
    Set.member position cells
