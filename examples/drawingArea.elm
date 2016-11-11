module Main exposing (..)

import Html as H exposing (Html)
import Html.App as App
import Html.Events as HE
import Html.Attributes as HA
import Json.Encode as JE
import DrawingArea as Area exposing (DrawingArea)
import Annotation as Ann exposing (Annotation)
import Tools exposing (Tool)


main =
    App.beginnerProgram
        { model = init
        , update = update
        , view = view
        }



-- MODEL #############################################################


type alias Model =
    { area : DrawingArea
    , current : Maybe ( Int, Annotation )
    , jsonExport : String
    }


init : Model
init =
    Model Area.default Nothing ""



-- UPDATE ############################################################


type Msg
    = NewAnnotation
    | Delete
    | Select ( Int, Annotation )
    | SelectTool Tool
    | ExportAnnotations


update : Msg -> Model -> Model
update msg model =
    case msg of
        NewAnnotation ->
            { model | area = Area.create model.area }

        Delete ->
            { model
                | current = Nothing
                , area =
                    case model.current of
                        Nothing ->
                            model.area

                        Just ( id, annotation ) ->
                            Area.remove id model.area
            }

        Select ( id, annotation ) ->
            { model | current = Just ( id, annotation ) }

        SelectTool tool ->
            { model | area = Area.useTool tool model.area }

        ExportAnnotations ->
            { model
                | jsonExport =
                    JE.encode 0 <| Area.exportSelectionsPaths model.area
            }



-- VIEW ##############################################################


view : Model -> Html Msg
view model =
    H.body []
        [ H.button [ HE.onClick NewAnnotation ] [ H.text "New Annotation" ]
        , H.text " Annotation: "
        , Area.selectAnnotationTag model.area model.current Select
        , H.button [ HE.onClick Delete ] [ H.text "Delete" ]
        , H.text " Tool: "
        , Area.selectToolTag model.area SelectTool
        , H.button [ HE.onClick ExportAnnotations ] [ H.text "Export" ]
        , H.br [] []
        , Area.view [] model.area
        , H.textarea [] [ H.text model.jsonExport ]
        , H.br [] []
        , H.text (toString model)
        ]
