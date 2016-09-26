module DrawingArea exposing
    (..)


import Json.Decode as Json


import Helpers as HP
import Image
import Annotation as Ann
import AnnotationSet as AnnSet
import Selections.RectangleSelection as RS
import Selections.OutlineSelection as OS




-- MODEL #############################################################




type Tool
    = RectangleTool
    | OutlineTool


type alias Model_ =
    { bgImage : Image.Model
    , annotations : AnnSet.Model
    , tool : Tool
    , zoomLevel : Float
    , origin : (Float, Float)
    , mouseDown : Bool
    , downPos : Maybe (Int, Int)
    }


type Model = DrawingArea Model_




-- UPDATE ############################################################




type Msg
    -- Zoom Management
    = ZoomIn
    | ZoomOut
    | Wheel Float
    -- Mouse Management
    | Down (Int, Int)
    | Move (Int, Int)
    | Up
    -- Background Image Management
    | ChangeImage Image.Model
    -- Annotations Management
    | CreateAnnotation
    | SelectTool Tool
    -- Other messages
    | Annotations AnnSet.Msg


update : Msg -> Model -> (Model, Cmd Msg)
update msg (DrawingArea model) =
    case msg of
        -- Zoom Management
        ZoomIn ->
            updateZoom 2 (DrawingArea model)
        ZoomOut ->
            updateZoom 0.5 (DrawingArea model)
        Wheel deltaY ->
            if deltaY > 0
            then updateZoom 0.5 (DrawingArea model)
            else updateZoom 2 (DrawingArea model)
        -- Mouse Management ->
        Down (x, y) ->
            ( DrawingArea {model | downPos = Just (x,y), mouseDown = True}
            , case model.tool of
                RectangleTool ->
                    rsCmd <| RS.Geom (Just (x,y)) Nothing
                OutlineTool ->
                    osCmd <| OS.ResetWithPoint (x,y)
            )
        Move (x', y') ->
            ( DrawingArea model
            , case model.tool of
                RectangleTool ->
                    let
                        (x,y) = Maybe.withDefault (0,0) model.downPos
                        left = min x x'
                        top = min y y'
                        width = abs (x-x')
                        height = abs (y-y')
                    in
                        rsCmd <| RS.Geom
                            (Just (left, top))
                            (Just (width, height))
                OutlineTool ->
                    osCmd <| OS.AddPoint (x',y')
            )
        Up ->
            ( DrawingArea {model | mouseDown = False, downPos = Nothing}
            , Cmd.none
            )
        -- Background Image Management ->
        ChangeImage imModel ->
            (DrawingArea model, Cmd.none)
        -- Annotations Management ->
        CreateAnnotation ->
            (DrawingArea model, Cmd.none)
        SelectTool tool ->
            (DrawingArea model, Cmd.none)
        -- Other messages ->
        Annotations annSetMsg ->
            (DrawingArea model, Cmd.none)


updateZoom : Float -> Model -> (Model, Cmd Msg)
updateZoom zoomModifier (DrawingArea model) =
    ( DrawingArea {model | zoomLevel = zoomModifier * model.zoomLevel}
    , Cmd.none
    )


selCmd : Ann.SelectionMsg -> Cmd Msg
selCmd =
    HP.msgToCmd
        << Annotations
        << AnnSet.Annotate
        << Ann.Selection
        << Just


rsCmd : RS.Msg -> Cmd Msg
rsCmd = selCmd << Ann.RSMsg


osCmd : OS.Msg -> Cmd Msg
osCmd = selCmd << Ann.OSMsg




-- VIEW ##############################################################



