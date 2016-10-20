module Cell exposing (..)

import Set exposing (Set)
import CellInit exposing (cells, altCells)


type alias Position =
    ( Int, Int )



-- Cell as position with neighbourCount?
-- Use Set functions instead of converting


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
    positionsToCheck (Set.toList cells)
        |> List.filterMap (\position -> updatePosition cells position)
        |> Set.fromList


positionsToCheck : List Position -> List Position
positionsToCheck cells =
    cells ++ List.concatMap possibleNeighbours cells


unique : List comparable -> List comparable
unique list =
    list
        |> Set.fromList
        |> Set.toList


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
    Set.filter (isCellAt cells) (Set.fromList (possibleNeighbours position))


possibleNeighbours : Position -> List Position
possibleNeighbours position =
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
