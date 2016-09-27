import Html as H
import Html.App as App
import Html.Events as HE
import Html.Attributes as HA


import DrawingArea
import Utils.Helpers as HP


main =
    App.program
        { init = init
        , update = update
        , subscriptions = (\_ -> Sub.none)
        , view = view
        }




-- MODEL #############################################################




type alias Model =
    { drawingArea : DrawingArea.Model
    }


init : (Model, Cmd Msg)
init =
    let
        (drawModel, drawCmd) = DrawingArea.init
    in
        ( Model drawModel
        , Cmd.map Draw drawCmd
        )




-- UPDATE ############################################################




type Msg
    = NewAnnotation
    | Delete
    | Select (Maybe Int)
    | ExportAnnotations
    | Draw DrawingArea.Msg


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NewAnnotation ->
            ( model
            , Cmd.map Draw <| HP.msgToCmd DrawingArea.CreateAnnotation
            )
        Delete ->
            ( model
            , Cmd.map Draw <| HP.msgToCmd DrawingArea.DeleteAnnotation
            )
        Select maybeId ->
            ( model
            , Cmd.map Draw <| HP.msgToCmd <| DrawingArea.SelectAnnotation maybeId
            )
        ExportAnnotations ->
            ( model
            , Cmd.map Draw <| HP.msgToCmd DrawingArea.ExportAnnotations
            )
        Draw drawMsg ->
            let
                (drawModel, drawCmd) = DrawingArea.update drawMsg model.drawingArea
            in
                ( {model | drawingArea = drawModel}
                , Cmd.map Draw drawCmd
                )




-- VIEW ##############################################################




view : Model -> H.Html Msg
view model =
    H.body []
        [ H.button [HE.onClick NewAnnotation] [H.text "New Annotation"]
        , H.button [HE.onClick Delete] [H.text "Delete"]
        , App.map Draw <| DrawingArea.selectHtml model.drawingArea
        , H.text " Tool: "
        , App.map Draw <| DrawingArea.selectToolView model.drawingArea
        , H.button [HE.onClick ExportAnnotations] [H.text "Export"]
        , H.br [] []
        , App.map Draw <| DrawingArea.view model.drawingArea
        , H.br [] []
        , H.text (toString model)
        ]

