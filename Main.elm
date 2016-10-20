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
            [ width "600", height "600", viewBox "0 0 200 200" ]
            (List.map displayCell (Set.toList model.cells))
        ]


cellSize : Int
cellSize =
    5


displayCell : Position -> Svg msg
displayCell cell =
    rect
        [ x (toString (fst cell * cellSize))
        , y (toString (snd cell * cellSize))
        , width (toString cellSize)
        , height (toString cellSize)
        ]
        []
