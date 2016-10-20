module Main exposing (..)

import Cell exposing (..)
import Set
import Html exposing (button, text, div, Html)
import Html.Events exposing (..)
import Html.App as Html
import Svg exposing (..)
import Svg.Attributes exposing (..)


type Msg
    = Tick


main : Program Never
main =
    Html.beginnerProgram
        { model = initModel
        , view = view
        , update = update
        }


initModel : Model
initModel =
    Cell.initModel


update : Msg -> Model -> Model
update msg model =
    case msg of
        Tick ->
            Cell.updateModel model


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Tick ] [ Html.text "Tick" ]
        , svg
            [ width "600", height "600" ]
            (List.map displayCell (Set.toList model.cells))
        ]


cellSize : Int
cellSize =
    5


yOffset : Int
yOffset =
    200


xOffset : Int
xOffset =
    200


displayCell : Position -> Svg msg
displayCell cell =
    rect
        [ x (toString (xOffset + fst cell * cellSize))
        , y (toString (yOffset + snd cell * cellSize))
        , width (toString cellSize)
        , height (toString cellSize)
        ]
        []
