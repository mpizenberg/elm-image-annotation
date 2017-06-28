module Main exposing (..)

import Html exposing (..)
import Html.Attributes as HtmlA exposing (..)


-- import Html.Events exposing (..)
-- import Svg exposing (..)
-- import Svg.Attributes as SvgA
-- import Annotation.Geometry.Types exposing (BoundingBox)
-- import Annotation.Geometry.BoundingBox as BoundingBox
-- import Annotation.Geometry.Point as Point
-- import Annotation.Svg as Svg
-- import Mouse
--
--


main =
    div [ id "annotation-app" ] [ toolbar, viewer ]


toolbar =
    div [ id "toolbar" ]
        [ tool "tool-grab-move"
        , tool "tool-bbox"
        , tool "control-zoom-in"
        , tool "control-zoom-out"
        , span [ class "text-tool" ] [ text "reset" ]
        , span [ class "text-tool" ] [ text "export annotations" ]
        ]


tool cssId =
    span [ id cssId, class "btn-tool" ] []


viewer =
    div [ id "viewer" ] []



-- main : Program Never Model Msg
-- main =
--     beginnerProgram
--         { model = model
--         , view = view
--         , update = update
--         }
--
--
-- type alias Model =
--     { bbox : Maybe BoundingBox
--     , dragState : DragState
--     }
--
--
-- type DragState
--     = Up
--     | DraggingFrom Mouse.Coordinates
--
--
-- model : Model
-- model =
--     Model Nothing Up
--
--
-- type Msg
--     = MouseDown Mouse.Coordinates
--     | MouseMove Mouse.Coordinates
--     | MouseUp
--
--
-- update : Msg -> Model -> Model
-- update msg model =
--     case ( msg, model.dragState ) of
--         ( MouseDown coordinates, _ ) ->
--             let
--                 point =
--                     Point.fromCoordinates coordinates
--
--                 bbox =
--                     BoundingBox.fromPair ( point, point )
--             in
--                 Model (Just bbox) (DraggingFrom coordinates)
--
--         ( MouseMove coordinates, DraggingFrom startCoordinates ) ->
--             let
--                 ( startPoint, point ) =
--                     ( Point.fromCoordinates startCoordinates
--                     , Point.fromCoordinates coordinates
--                     )
--
--                 bbox =
--                     BoundingBox.fromPair ( startPoint, point )
--             in
--                 Model (Just bbox) (DraggingFrom startCoordinates)
--
--         ( MouseUp, _ ) ->
--             { model | dragState = Up }
--
--         _ ->
--             model
--
--
-- view : Model -> Html Msg
-- view model =
--     div (HtmlA.style [ ( "height", "98%" ) ] :: mouseEvents model.dragState)
--         [ svg [ SvgA.style "width: 100%; height: 100%;" ]
--             [ viewBBox model.bbox ]
--         ]
--
--
-- viewBBox : Maybe BoundingBox -> Svg msg
-- viewBBox maybeBBox =
--     maybeBBox
--         |> Maybe.map Svg.boundingBox
--         |> Maybe.withDefault (Svg.text "No bounding box")
--
--
-- mouseEvents : DragState -> List (Html.Attribute Msg)
-- mouseEvents dragState =
--     case dragState of
--         Up ->
--             [ Mouse.onDown (.clientPos >> MouseDown)
--             , onMouseUp MouseUp
--             ]
--
--         _ ->
--             [ Mouse.onDown (.clientPos >> MouseDown)
--             , Mouse.onMove (.clientPos >> MouseMove)
--             , onMouseUp MouseUp
--             ]
