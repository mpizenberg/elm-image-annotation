module DrawingArea exposing
    (..)


import Svg
import Svg.Attributes as SvgA
import Html.Attributes as HA
import Json.Decode as Json


import Utils.Helpers as HP
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
    { bgImage : Maybe Image.Model
    , annotations : AnnSet.Model
    , tool : Tool
    , zoomLevel : Float
    , origin : (Float, Float)
    , mouseDown : Bool
    , downPos : Maybe (Int, Int)
    }


type Model = DrawingArea Model_


init : (Model, Cmd Msg)
init =
    ( DrawingArea <| Model_
        Nothing -- bgImage
        (fst AnnSet.init)
        RectangleTool
        1.0 -- zoomLevel
        (0,0) -- origin
        False -- mouseDown
        Nothing -- downPos
    , Cmd.none
    )




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
    | ChangeImage (Maybe Image.Model)
    -- Annotations Management
    | CreateAnnotation
    | DeleteAnnotation
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
            ( DrawingArea {model | bgImage = imModel}
            , Cmd.none
            )
        -- Annotations Management ->
        CreateAnnotation ->
            ( DrawingArea model
            , Cmd.map Annotations <| HP.msgToCmd AnnSet.CreateAnnotation
            )
        DeleteAnnotation ->
            ( DrawingArea model
            , Cmd.map Annotations <| HP.msgToCmd AnnSet.Delete
            )
        SelectTool tool ->
            ( DrawingArea {model | tool = tool}
            , Cmd.none
            )
        -- Other messages ->
        Annotations annSetMsg ->
            let
                ( annSetModel, cmd ) =
                    AnnSet.update annSetMsg model.annotations
            in
                ( DrawingArea {model | annotations = annSetModel}
                , Cmd.map Annotations cmd
                )


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




view : Model -> Svg.Svg Msg
view (DrawingArea model) =
    Svg.svg
        ([ svgTransform model.zoomLevel model.origin
        , drawingAreaStyle ]
        ++ offsetsEvents model.mouseDown
        )
        ( ( case model.bgImage of
            Nothing -> []
            Just image ->
                [ Image.view
                    Image.SvgTag
                    (Just "bgImage")
                    Nothing
                    image
                ]
          )
        ++ AnnSet.selectionsView model.annotations
        )




-- VIEW HELPERS ######################################################




svgTransform : Float -> (Float, Float) -> Svg.Attribute msg
svgTransform zoomLevel (x,y) =
    SvgA.transform <|
        "scale(" ++ toString zoomLevel ++ ") " ++
        "translate" ++ toString (-x,-y)


drawingAreaStyle : Svg.Attribute msg
drawingAreaStyle =
    HA.style
        [ ("display", "inline-block")
        , ("border", "1px solid")
        , ("width", "800px")
        , ("height", "400px")
        ]


offsetsEvents : Bool -> List (Svg.Attribute Msg)
offsetsEvents down =
    let
        baseOffsets = [(HP.offsetOn "mousedown") Down]
    in
        if down
        then (HP.offsetOn "mousemove") Move :: baseOffsets
        else baseOffsets
