module Contour exposing (..)

import Html exposing (..)
import Html.Attributes as HtmlA exposing (..)
import Svg exposing (..)
import Svg.Attributes as SvgA
import Annotation.Geometry.Types exposing (Contour, Point, Stroke)
import Annotation.Geometry.Contour as Contour
import Annotation.Geometry.Point as Point
import Annotation.Geometry.Stroke as Stroke
import Annotation.Svg as Svg
import Mouse
import Keyboard


main : Program Never Model Msg
main =
    program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { contourState : ContourState
    }


type ContourState
    = None
    | AddingPoint Stroke Point
    | Building Stroke
    | Built Contour


init : ( Model, Cmd Msg )
init =
    ( Model None, Cmd.none )


type Msg
    = MouseDown Point
    | MouseMove Point
    | MouseUp Point
    | KeyUp Keyboard.KeyCode


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( updateModel msg model, Cmd.none )


updateModel : Msg -> Model -> Model
updateModel msg model =
    case ( msg, model.contourState ) of
        ( MouseDown nextPoint, None ) ->
            Model (AddingPoint Stroke.empty nextPoint)

        ( MouseMove nextPoint, AddingPoint stroke _ ) ->
            Model (AddingPoint stroke nextPoint)

        ( MouseUp nextPoint, AddingPoint stroke _ ) ->
            Model <| Building (Stroke.addPoint nextPoint stroke)

        ( MouseDown nextPoint, Building stroke ) ->
            Model (AddingPoint stroke nextPoint)

        ( KeyUp 13, Building stroke ) ->
            Model <| Built (Stroke.close stroke)

        ( MouseDown nextPoint, Built _ ) ->
            updateModel (MouseDown nextPoint) (Model None)

        _ ->
            model


view : Model -> Html Msg
view model =
    div (HtmlA.style [ ( "height", "98%" ) ] :: mouseEvents model.contourState)
        [ svg [ SvgA.style "width: 100%; height: 100%;" ]
            [ viewContour model.contourState ]
        , p [ HtmlA.style [ ( "position", "fixed" ), ( "top", "0px" ) ] ]
            [ Html.text "Start contour by clicking somewhere. Hit enter to close it" ]
        ]


viewContour : ContourState -> Svg msg
viewContour contourState =
    case contourState of
        None ->
            Svg.text "No contour started"

        AddingPoint stroke point ->
            Svg.g []
                [ Svg.stroke (Stroke.addPoint point stroke)
                , Svg.point point
                ]

        Building stroke ->
            Svg.stroke stroke

        Built contour ->
            Svg.contour contour


mouseEvents : ContourState -> List (Html.Attribute Msg)
mouseEvents contourState =
    case contourState of
        AddingPoint _ _ ->
            [ Mouse.onDown (.clientPos >> Point.fromCoordinates >> MouseDown)
            , Mouse.onMove (.clientPos >> Point.fromCoordinates >> MouseMove)
            , Mouse.onUp (.clientPos >> Point.fromCoordinates >> MouseUp)
            ]

        _ ->
            [ Mouse.onDown (.clientPos >> Point.fromCoordinates >> MouseDown)
            , Mouse.onUp (.clientPos >> Point.fromCoordinates >> MouseUp)
            ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Keyboard.ups KeyUp
