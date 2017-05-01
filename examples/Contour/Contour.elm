module Contour exposing (..)

import Html exposing (..)
import Html.Attributes as HtmlA exposing (..)
import Svg exposing (..)
import Svg.Attributes as SvgA
import Annotation.Geometry.Types exposing (Contour, Point)
import Annotation.Geometry.Contour as Contour
import Annotation.Geometry.Point as Point
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
    { contour : Maybe Contour
    , nextPoint : Maybe Point
    , contourState : ContourState
    }


type ContourState
    = NotBuilding
    | Building


model : Model
model =
    Model Nothing Nothing NotBuilding


init : ( Model, Cmd Msg )
init =
    ( model, Cmd.none )


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
    case ( msg, model.contourState, model.contour ) of
        ( MouseDown nextPoint, NotBuilding, _ ) ->
            Model (Just Contour.empty) (Just nextPoint) Building

        ( MouseMove nextPoint, Building, _ ) ->
            { model | nextPoint = Just nextPoint }

        ( MouseUp nextPoint, Building, Just contour ) ->
            { model | contour = Just <| Contour.addPoint nextPoint contour }

        ( KeyUp 13, Building, _ ) ->
            { model | contourState = NotBuilding }

        _ ->
            model


view : Model -> Html Msg
view model =
    div (HtmlA.style [ ( "height", "98%" ) ] :: mouseEvents model.contourState)
        [ svg [ SvgA.style "width: 100%; height: 100%;" ]
            [ viewContour model.contourState model.nextPoint model.contour ]
        , p [ HtmlA.style [ ( "position", "fixed" ), ( "top", "0px" ) ] ]
            [ Html.text "Start contour by clicking somewhere. Hit enter to close it" ]
        ]


viewContour : ContourState -> Maybe Point -> Maybe Contour -> Svg msg
viewContour state maybePoint maybeContour =
    case ( state, maybeContour, maybePoint ) of
        ( _, Nothing, _ ) ->
            Svg.text "No contour started"

        ( NotBuilding, Just contour, _ ) ->
            Svg.contour contour

        ( Building, Just contour, Just nextPoint ) ->
            Svg.contourBuilding nextPoint contour

        _ ->
            Svg.text "Incorrect state"


mouseEvents : ContourState -> List (Html.Attribute Msg)
mouseEvents contourState =
    case contourState of
        NotBuilding ->
            [ Mouse.onDown (.clientPos >> Point.fromCoordinates >> MouseDown)
            , Mouse.onUp (.clientPos >> Point.fromCoordinates >> MouseUp)
            ]

        Building ->
            [ Mouse.onDown (.clientPos >> Point.fromCoordinates >> MouseDown)
            , Mouse.onMove (.clientPos >> Point.fromCoordinates >> MouseMove)
            , Mouse.onUp (.clientPos >> Point.fromCoordinates >> MouseUp)
            ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Keyboard.ups KeyUp
