module Main exposing (..)

import Cell exposing (..)
import Set
import Html exposing (button, text, div, Html)
import Html.Events exposing (..)
import Html.App as Html
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time)


type Msg
    = Tick Time


main : Program Never
main =
    Html.program
        { init = initModel
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


initModel : ( Model, Cmd msg )
initModel =
    Cell.initModel ! []


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        Tick time ->
            Cell.updateModel model ! []


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick (Tick 0) ] [ Html.text "Tick" ]
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


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (100 * Time.millisecond) Tick


displayCell : Position -> Svg msg
displayCell cell =
    rect
        [ x (toString (xOffset + fst cell * cellSize))
        , y (toString (yOffset + snd cell * cellSize))
        , width (toString cellSize)
        , height (toString cellSize)
        ]
        []
