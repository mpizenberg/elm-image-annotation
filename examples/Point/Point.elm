module Point exposing (..)

import Html exposing (..)
import Html.Attributes as HtmlA exposing (..)
import Html.Events exposing (..)
import Svg exposing (..)
import Svg.Attributes as SvgA
import Annotation.Geometry.Types exposing (Point)
import Annotation.Geometry.Point as Point
import Annotation.Svg as Svg
import Annotation.Style as Style
import Annotation.Color as Color
import Mouse


main : Program Never Model Msg
main =
    beginnerProgram
        { model = model
        , view = view
        , update = update
        }


type alias Model =
    { point : Maybe Point
    , mouseState : MouseState
    }


type MouseState
    = Up
    | Down


model : Model
model =
    Model Nothing Up


type Msg
    = MouseAt Mouse.Coordinates
    | MouseUp


update : Msg -> Model -> Model
update msg model =
    case msg of
        MouseAt coordinates ->
            Model (Just <| Point.fromCoordinates coordinates) Down

        MouseUp ->
            { model | mouseState = Up }


view : Model -> Html Msg
view model =
    div (HtmlA.style [ ( "height", "98%" ) ] :: mouseEvents model.mouseState)
        [ svg [ SvgA.style "width: 100%; height: 100%;" ]
            [ viewPoint model.point ]
        ]


viewPoint : Maybe Point -> Svg msg
viewPoint maybePoint =
    maybePoint
        |> Maybe.map (Svg.pointStyled <| Style.Disk 10 Color.turquoise)
        |> Maybe.withDefault (Svg.text "No point")


mouseEvents : MouseState -> List (Html.Attribute Msg)
mouseEvents mouseState =
    case mouseState of
        Up ->
            [ Mouse.onDown (.clientPos >> MouseAt)
            , onMouseUp MouseUp
            ]

        Down ->
            [ Mouse.onDown (.clientPos >> MouseAt)
            , Mouse.onMove (.clientPos >> MouseAt)
            , onMouseUp MouseUp
            ]
