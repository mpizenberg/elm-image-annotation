module Main exposing (..)

import Array exposing (Array)
import Html as H exposing (Html)
import Html.App as App
import Html.Events as HE
import Html.Attributes as HA
import Json.Encode as JE
import DrawingArea as Area exposing (DrawingArea)
import Annotation as Ann exposing (Annotation)
import Tools exposing (Tool)
import Pointer exposing (Pointer)
import Image exposing (Image)


main =
    App.beginnerProgram
        { model = init
        , update = update
        , view = view
        }



-- MODEL #############################################################


type ZoomVariation
    = ZoomIn
    | ZoomOut


type alias Model =
    { area : DrawingArea
    , current : Maybe ( Int, Annotation )
    , jsonExport : String
    , pointer : Maybe Pointer
    , downOrigin : ( Float, Float )
    , label : String
    }


init : Model
init =
    Model
        (Area.default
            |> Area.changeBgImage (Just (Image "http://lorempixel.com/200/200" 200 200))
            |> Area.fitImage 0.8
        )
        Nothing
        ""
        Nothing
        ( 0, 0 )
        ""



-- UPDATE ############################################################


type Msg
    = NewAnnotation
    | Delete
    | Select (Maybe ( Int, Annotation ))
    | SelectTool Tool
    | ExportAnnotations
    | PointerEvent Pointer
    | Zoom ZoomVariation
    | ChangeLabel String
    | ApplyLabel


update : Msg -> Model -> Model
update msg model =
    case msg of
        NewAnnotation ->
            let
                area =
                    Area.create model.area

                length =
                    Array.length area.annotations
            in
                { model | area = area, current = Area.get (length - 1) area }

        Delete ->
            case model.current of
                Nothing ->
                    model

                Just ( id, annotation ) ->
                    let
                        area =
                            Area.remove id model.area

                        current =
                            Area.get 0 area
                    in
                        { model | area = area, current = current }

        Select maybeItem ->
            { model | current = maybeItem }

        SelectTool tool ->
            { model | area = Area.useTool tool model.area }

        ExportAnnotations ->
            { model
                | jsonExport =
                    JE.encode 0 <| Area.exportSelectionsPaths model.area
            }

        PointerEvent pointer ->
            let
                downOrigin =
                    case pointer.event of
                        Pointer.Down ->
                            ( pointer.offsetX, pointer.offsetY )

                        _ ->
                            model.downOrigin

                ( newCurrent, newArea ) =
                    Area.updateAnnotations downOrigin pointer model.current model.area
            in
                { model
                    | pointer = updatePointer pointer
                    , area = newArea
                    , current = newCurrent
                    , downOrigin = downOrigin
                }

        Zoom var ->
            case var of
                ZoomIn ->
                    { model | area = Area.zoomIn model.area }

                ZoomOut ->
                    { model | area = Area.zoomOut model.area }

        ChangeLabel label ->
            { model | label = label }

        ApplyLabel ->
            case model.current of
                Nothing ->
                    model

                Just ( id, annotation ) ->
                    let
                        ann =
                            { annotation | label = model.label }

                        current =
                            Just ( id, ann )

                        area =
                            Area.set id ann model.area
                    in
                        { model | area = area, current = current }


updatePointer : Pointer -> Maybe Pointer
updatePointer pointer =
    case pointer.event of
        Pointer.Down ->
            Just pointer

        Pointer.Move ->
            Just pointer

        _ ->
            Nothing



-- VIEW ##############################################################


view : Model -> Html Msg
view model =
    H.body []
        [ H.button [ HE.onClick NewAnnotation ] [ H.text "New Annotation" ]
        , H.text " Annotation: "
        , Area.selectAnnotationTag model.area model.current Select
        , H.input [ HA.type' "text", HA.placeholder "Label", HE.onInput ChangeLabel ] []
        , H.button [ HE.onClick ApplyLabel ] [ H.text "Apply Label" ]
        , H.button [ HE.onClick Delete ] [ H.text "Delete" ]
        , H.text " Tool: "
        , Area.selectToolTag model.area SelectTool
        , H.button [ HE.onClick <| Zoom ZoomIn ] [ H.text "Zoom In" ]
        , H.button [ HE.onClick <| Zoom ZoomOut ] [ H.text "Zoom Out" ]
        , H.button [ HE.onClick ExportAnnotations ] [ H.text "Export" ]
        , H.br [] []
        , let
            annotation =
                case model.current of
                    Nothing ->
                        Nothing

                    Just ( id, ann ) ->
                        Just ann
          in
            Area.viewAnnotation
                (pointerEventAttributes model.area.currentTool model.pointer)
                annotation
                model.area
        , H.textarea [] [ H.text model.jsonExport ]
        , H.br [] []
        , H.text (toString model)
        ]


pointerEventAttributes : Tool -> Maybe Pointer -> List (H.Attribute Msg)
pointerEventAttributes =
    Pointer.attributes PointerEvent
