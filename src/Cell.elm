module Cell exposing (..)

import Set exposing (Set)


type alias Position =
    ( Int, Int )



--Cell as position with neighbourCount?
--SetOfCells instead of a list? as position with neighbourCount?


type alias NeighbourCount =
    Int


type alias Model =
    { cells : Set Position }


updateModel : Model -> Model
updateModel model =
    { cells = updatePositions model.cells }


updatePositions : Set Position -> Set Position
updatePositions cells =
    Set.fromList
        (List.filterMap
            (\position ->
                updatePosition (Set.toList cells) position (List.length (neighbours (Set.toList cells) position))
            )
            (positionsToCheck (Set.toList cells))
        )


positionsToCheck : List Position -> List Position
positionsToCheck cells =
    unique (cells ++ List.concatMap possibleNeighbours cells)


unique : List comparable -> List comparable
unique list =
    list
        |> Set.fromList
        |> Set.toList


updatePosition : List Position -> Position -> NeighbourCount -> Maybe Position
updatePosition cells position neighbourCount =
    if neighbourCount < 2 then
        Nothing
    else if neighbourCount == 3 then
        Just position
    else if neighbourCount == 2 && isCellAt cells position then
        Just position
    else
        Nothing


neighbours : List Position -> Position -> List Position
neighbours cellList position =
    List.filter (isCellAt cellList) (possibleNeighbours position)


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


isCellAt : List Position -> Position -> Bool
isCellAt model position =
    List.member position model
