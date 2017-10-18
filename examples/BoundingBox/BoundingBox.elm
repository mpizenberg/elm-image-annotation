module BoundingBox exposing (..)

import Annotation.Geometry.BoundingBox as BoundingBox
import Annotation.Geometry.Point as Point
import Annotation.Geometry.Types exposing (BoundingBox)
import Annotation.Svg as Svg
import Annotation.Viewer as Viewer exposing (Viewer)
import Html exposing (..)
import Html.Attributes as HtmlA exposing (..)
import Pointer
import Svg exposing (..)


main : Program Never Model Msg
main =
    beginnerProgram
        { model = model
        , view = view
        , update = update
        }


type alias Model =
    { bbox : Maybe BoundingBox
    , dragState : DragState
    , viewer : Viewer
    }


type DragState
    = Up
    | DraggingFrom ( Float, Float )


model : Model
model =
    Model Nothing Up Viewer.default


type Msg
    = PointerDownAt ( Float, Float )
    | PointerMoveAt ( Float, Float )
    | PointerUp


update : Msg -> Model -> Model
update msg model =
    case ( msg, model.dragState ) of
        ( PointerDownAt coordinates, _ ) ->
            let
                point =
                    Point.fromCoordinates coordinates

                bbox =
                    BoundingBox.fromPair ( point, point )
            in
            Model (Just bbox) (DraggingFrom coordinates) Viewer.default

        ( PointerMoveAt coordinates, DraggingFrom startCoordinates ) ->
            let
                ( startPoint, point ) =
                    ( Point.fromCoordinates startCoordinates
                    , Point.fromCoordinates coordinates
                    )

                bbox =
                    BoundingBox.fromPair ( startPoint, point )
            in
            { model | bbox = Just bbox }

        ( PointerUp, _ ) ->
            { model | dragState = Up }

        _ ->
            model


view : Model -> Html Msg
view model =
    let
        htmlAttributes =
            [ id "viewer"

            -- use the elm-pep polyfill
            , attribute "elm-pep" "true"

            -- prevent default browser scrolls
            , HtmlA.style [ ( "touch-action", "none" ) ]
            ]
    in
    Viewer.viewInWithDetails
        (htmlAttributes ++ pointerEvents)
        model.viewer
        (viewBBox model.bbox)


viewBBox : Maybe BoundingBox -> Svg msg
viewBBox maybeBBox =
    maybeBBox
        |> Maybe.map Svg.boundingBox
        |> Maybe.withDefault (Svg.text "No bounding box")


pointerEvents : List (Html.Attribute Msg)
pointerEvents =
    [ Pointer.onDown (.pointer >> .offsetPos >> PointerDownAt)
    , Pointer.onMove (.pointer >> .offsetPos >> PointerMoveAt)
    , Pointer.onUp (always PointerUp)
    ]
