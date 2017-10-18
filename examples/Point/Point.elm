module Point exposing (..)

import Annotation.Color as Color
import Annotation.Geometry.Point as Point
import Annotation.Geometry.Types exposing (Point)
import Annotation.Style as Style
import Annotation.Svg as Svg
import Annotation.Viewer as Viewer exposing (Viewer)
import Html exposing (..)
import Html.Attributes exposing (..)
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
    { point : Maybe Point
    , pointerState : PointerState
    , viewer : Viewer
    }


type PointerState
    = Up
    | Down


model : Model
model =
    Model Nothing Up Viewer.default


type Msg
    = PointerDownAt ( Float, Float )
    | PointerMoveAt ( Float, Float )
    | PointerUp


update : Msg -> Model -> Model
update msg model =
    case ( msg, model.pointerState ) of
        ( PointerDownAt coordinates, _ ) ->
            Model (Just <| Point.fromCoordinates coordinates) Down Viewer.default

        ( PointerMoveAt coordinates, Down ) ->
            { model | point = Just (Point.fromCoordinates coordinates) }

        ( PointerUp, _ ) ->
            { model | pointerState = Up }

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
            , Html.Attributes.style [ ( "touch-action", "none" ) ]
            ]
    in
    Viewer.viewInWithDetails
        (htmlAttributes ++ pointerEvents)
        model.viewer
        (viewPoint model.point)


viewPoint : Maybe Point -> Svg msg
viewPoint maybePoint =
    maybePoint
        |> Maybe.map (Svg.pointStyled <| Style.Disk 50 Color.turquoise)
        |> Maybe.withDefault (Svg.text "No point")


pointerEvents : List (Html.Attribute Msg)
pointerEvents =
    [ Pointer.onDown (.pointer >> .offsetPos >> PointerDownAt)
    , Pointer.onMove (.pointer >> .offsetPos >> PointerMoveAt)
    , Pointer.onUp (always PointerUp)
    ]
