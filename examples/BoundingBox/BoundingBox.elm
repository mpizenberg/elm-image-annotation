module BoundingBox exposing (..)

import Html exposing (..)
import Html.Attributes as HtmlA exposing (..)
import Html.Events exposing (..)
import Svg exposing (..)
import Svg.Attributes as SvgA
import Annotation.Geometry.Types exposing (BoundingBox)
import Annotation.Geometry.BoundingBox as BoundingBox
import Annotation.Geometry.Point as Point
import Annotation.Svg as Svg
import Json.Decode as Decode exposing (Decoder)


main : Program Never Model Msg
main =
    beginnerProgram
        { model = model
        , view = view
        , update = update
        }


type alias Model =
    { bbox : Maybe BoundingBox
    , pointer : Pointer
    }


type Pointer
    = Up
    | DraggingFrom Position


type alias Position =
    ( Float, Float )


model : Model
model =
    Model Nothing Up


type Msg
    = MouseDown Position
    | MouseMove Position
    | MouseUp


update : Msg -> Model -> Model
update msg model =
    case ( msg, model.pointer ) of
        ( MouseDown pos, _ ) ->
            let
                point =
                    Point.fromCoordinates pos

                bbox =
                    BoundingBox.fromPair ( point, point )
            in
                Model (Just bbox) (DraggingFrom pos)

        ( MouseMove pos, DraggingFrom startPos ) ->
            let
                ( startPoint, point ) =
                    ( Point.fromCoordinates startPos
                    , Point.fromCoordinates pos
                    )

                bbox =
                    BoundingBox.fromPair ( startPoint, point )
            in
                Model (Just bbox) (DraggingFrom startPos)

        ( MouseUp, _ ) ->
            { model | pointer = Up }

        _ ->
            model


view : Model -> Html Msg
view model =
    div (HtmlA.style [ ( "height", "98%" ) ] :: mouseEvents model.pointer)
        [ svg [ SvgA.style "width: 100%; height: 100%;" ]
            [ viewBBox model.bbox ]
        ]


viewBBox : Maybe BoundingBox -> Svg msg
viewBBox maybeBBox =
    maybeBBox
        |> Maybe.map Svg.boundingBox
        |> Maybe.withDefault (Svg.text "No bounding box")


mouseEvents : Pointer -> List (Html.Attribute Msg)
mouseEvents pointer =
    case pointer of
        Up ->
            [ positionOn "mousedown" MouseDown
            , onMouseUp MouseUp
            ]

        _ ->
            [ positionOn "mousedown" MouseDown
            , positionOn "mousemove" MouseMove
            , onMouseUp MouseUp
            ]


positionOn : String -> (Position -> Msg) -> Html.Attribute Msg
positionOn mouseEvent tagger =
    onWithOptions mouseEvent stop decoderPos
        |> HtmlA.map tagger


decoderPos : Decoder Position
decoderPos =
    Decode.map2 (,)
        (Decode.field "clientX" Decode.float)
        (Decode.field "clientY" Decode.float)


stop : { preventDefault : Bool, stopPropagation : Bool }
stop =
    { stopPropagation = True
    , preventDefault = True
    }
