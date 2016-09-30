module DrawingArea exposing
    ( Tool(..), Model, init
    , Msg(..), update
    , view, viewWithImage, selectHtml, selectToolView
    , exportAnnotations, exportSelectionsPaths
    , hasSelection
    )

{-| The DrawingArea module aims at collecting annotations.

# Model
@docs Tool, Model, init

# Update
@docs Msg, update

# View
@docs view, viewWithImage, selectHtml, selectToolView

# Outputs
@docs exportAnnotations, exportSelectionsPaths

# Other
@docs hasSelection

-}

import Svg
import Svg.Attributes as SvgA
import Html as H
import Html.App as App
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as Json
import Json.Encode as JE
import String


import Utils.Helpers as HP
import Image
import Annotation as Ann
import AnnotationSet as AnnSet
import Selections.RectangleSelection as RS
import Selections.OutlineSelection as OS




-- MODEL #############################################################




{-| The type of tool that can be used to draw selections -}
type Tool
    = NoTool
    | RectangleTool
    | OutlineTool


type alias Model_ =
    { bgImage : Maybe Image.Model
    , annotations : AnnSet.Model
    , tool : Tool
    -- Area Management
    , size : (Float, Float)
    , zoomLevel : Float
    , origin : (Float, Float)
    -- Mouse Management
    , mouseDown : Bool
    , downPos : Maybe (Int, Int)
    }


{-| The model to manipulate drawing areas -}
type Model = DrawingArea Model_


{-| Initialize a DrawingArea.Model -}
init : (Model, Cmd Msg)
init =
    ( DrawingArea <| Model_
        Nothing -- bgImage
        (fst AnnSet.init)
        NoTool
        (800,400) -- (width, height)
        1.0 -- zoomLevel
        (0,0) -- origin
        False -- mouseDown
        Nothing -- downPos
    , Cmd.none
    )




-- UPDATE ############################################################




{-| The messages used to Manipulate a DrawingArea -}
type Msg
    -- Area Management
    = ZoomIn
    | ZoomOut
    | Wheel Float
    | ChangeZoomLevel Float
    | ChangeOrigin (Float, Float)
    | Center (Float, Float)
    -- Mouse Management
    | Down (Int, Int)
    | Move (Int, Int)
    | Up
    -- Background Image Management
    | ChangeImage (Maybe Image.Model)
    | CenterImage (Int, Int)
    -- Annotations Management
    | CreateAnnotation
    | DeleteAnnotation
    | ResetAnnotation
    | SelectAnnotation (Maybe Int)
    | SelectTool Tool
    -- Other messages
    | Annotations AnnSet.Msg


{-| Update a DrawingArea model -}
update : Msg -> Model -> (Model, Cmd Msg)
update msg (DrawingArea model) =
    case msg of
        -- Area Management
        ZoomIn ->
            updateZoom 2 (DrawingArea model)
        ZoomOut ->
            updateZoom 0.5 (DrawingArea model)
        Wheel deltaY ->
            if deltaY > 0
            then updateZoom 0.5 (DrawingArea model)
            else updateZoom 2 (DrawingArea model)
        ChangeZoomLevel zoom ->
            ( DrawingArea {model | zoomLevel = zoom}
            , Cmd.none
            )
        ChangeOrigin (left, top) ->
            ( DrawingArea {model | origin = (left, top)}
            , Cmd.none
            )
        Center (cx, cy) ->
            let
                (width, height) = model.size
            in
                ( DrawingArea {model | origin =
                    ( cx - 0.5 * width / model.zoomLevel
                    , cy - 0.5 * height / model.zoomLevel
                    )}
                , Cmd.none
                )
        -- Mouse Management ->
        Down (x, y) ->
            let
                (ox, oy) = model.origin
                x'' = round <| ox + (toFloat x) / model.zoomLevel
                y'' = round <| oy + (toFloat y) / model.zoomLevel
            in
                ( DrawingArea
                    {model | downPos = Just (x'', y''), mouseDown = True}
                , case model.tool of
                    NoTool ->
                        Cmd.none
                    RectangleTool ->
                        rsCmd <| RS.Geom (Just (x'', y'')) (Just (0,0))
                    OutlineTool ->
                        osCmd <| OS.ResetWithPoint (x'', y'')
                )
        Move (x', y') ->
            ( DrawingArea model
            , case model.tool of
                NoTool ->
                    let
                        (x,y) = model.origin
                    in
                        HP.msgToCmd <| ChangeOrigin
                            ( x - toFloat x' / model.zoomLevel
                            , y - toFloat y' / model.zoomLevel
                            )
                RectangleTool ->
                    let
                        (x,y) = Maybe.withDefault (0,0) model.downPos
                        (ox, oy) = model.origin
                        x'' = round <| ox + (toFloat x') / model.zoomLevel
                        y'' = round <| oy + (toFloat y') / model.zoomLevel
                        left = min x x''
                        top = min y y''
                        width = abs (x-x'')
                        height = abs (y-y'')
                    in
                        rsCmd <| RS.Geom
                            (Just (left, top))
                            (Just (width, height))
                OutlineTool ->
                    let
                        (ox, oy) = model.origin
                        x'' = round <| ox + (toFloat x') / model.zoomLevel
                        y'' = round <| oy + (toFloat y') / model.zoomLevel
                    in
                        osCmd <| OS.AddPoint (x'',y'')
            )
        Up ->
            ( DrawingArea {model | mouseDown = False, downPos = Nothing}
            , Cmd.none
            )
        -- Background Image Management ->
        ChangeImage imModel ->
            ( DrawingArea {model | bgImage = imModel}
            , case imModel of
                Nothing -> Cmd.none
                Just im ->
                    HP.msgToCmd <| CenterImage <| Image.size im
            )
        CenterImage (imageWidth, imageHeight) ->
            let
                (w, h) = model.size
                iw = toFloat imageWidth
                ih = toFloat imageHeight
                zoom = 0.8 * min (w/iw) (h/ih)
                ox = 0.5 * (iw - w / zoom)
                oy = 0.5 * (ih - h / zoom)
            in
                ( DrawingArea {model | origin = (ox,oy), zoomLevel = zoom}
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
        ResetAnnotation ->
            ( DrawingArea model
            , Cmd.map Annotations <| HP.msgToCmd AnnSet.ResetAnnotation
            )
        SelectAnnotation maybeId ->
            ( DrawingArea model
            , Cmd.map Annotations <| HP.msgToCmd <| AnnSet.Select maybeId
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
    let
        (left, top) = model.origin
        (width, height) = model.size
        centerX = left + 0.5 * width / model.zoomLevel
        centerY = top + 0.5 * height / model.zoomLevel
    in
        ( DrawingArea {model | zoomLevel = zoomModifier * model.zoomLevel}
        , HP.msgToCmd <| Center (centerX, centerY)
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




{-| View the svg tag representing the DrawingArea model -}
view : Model -> Svg.Svg Msg
view (DrawingArea model) =
    Svg.svg
        ([ HE.onMouseUp Up
        , onWheel Wheel
        , svgTransform model.zoomLevel model.origin
        , drawingAreaStyle model.size ]
        ++ offsetsEvents model.mouseDown model.tool
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


{-| Same as view but with a background image given as argument -}
viewWithImage : Maybe Image.Model -> Model -> Svg.Svg Msg
viewWithImage maybeImage model =
    view <| fst <| update (ChangeImage maybeImage) model




-- VIEW HELPERS ######################################################




svgTransform : Float -> (Float, Float) -> Svg.Attribute msg
svgTransform zoomLevel (x,y) =
    SvgA.transform <|
        "scale(" ++ toString zoomLevel ++ ") " ++
        "translate" ++ toString (-x,-y)


drawingAreaStyle : (Float, Float) -> Svg.Attribute msg
drawingAreaStyle (width, height) =
    HA.style
        [ ("display", "inline-block")
        , ("border", "1px solid")
        , ("width", toString width ++ "px")
        , ("height", toString height ++ "px")
        ]


offsetsEvents : Bool -> Tool -> List (Svg.Attribute Msg)
offsetsEvents down tool =
    let
        baseOffsets = [(HP.offsetOn "mousedown") Down]
    in
        baseOffsets
        ++ ( case (down, tool) of
            (True, NoTool) ->
                [(HP.movementOn "mousemove") Move]
            (True, _) ->
                [(HP.offsetOn "mousemove") Move]
            (False, _) -> []
        )


{-| Create a select form tag to change dynamically the current annotation -}
selectHtml : Model -> H.Html Msg
selectHtml (DrawingArea model) =
    App.map Annotations <| AnnSet.selectHtml model.annotations


{- Get the wheel deltaY attribute of a mouse event -}
onWheel : ((Float -> msg) -> H.Attribute msg)
onWheel =
    HP.specialOn "wheel" deltaYDecoder identity


deltaYDecoder : Json.Decoder Float
deltaYDecoder = Json.at ["deltaY"] Json.float


{-| A select form tag to change dynamically the current Tool -}
selectToolView : Model -> H.Html Msg
selectToolView (DrawingArea model) =
    H.select
        [HP.onChange <| SelectTool << optionToTool]
        (List.map
            (toolOptionTag model.tool)
            [ (NoTool, "None")
            , (RectangleTool, "Rectangle")
            , (OutlineTool, "Outline")
            ]
        )


optionToTool: String -> Tool
optionToTool id =
    let
        id' = String.toInt id
    in
        case id' of
            Err _ -> NoTool
            Ok toolId ->
                case toolId of
                    1 -> RectangleTool
                    2 -> OutlineTool
                    _ -> NoTool


toolOptionTag : Tool -> (Tool, String) -> H.Html Msg
toolOptionTag currentTool (tool, message) =
    H.option
        [ HA.value <| case tool of
            NoTool -> toString 0
            RectangleTool -> toString 1
            OutlineTool -> toString 2
        , HA.selected (currentTool == tool)
        ]
        [ H.text message
        ]




-- OUTPUTS ##############################################################




{-| Export the complete model of the annotation set to a JS object -}
exportAnnotations : Model -> JE.Value
exportAnnotations (DrawingArea model) =
    AnnSet.object model.annotations


{-| Export only the seletions paths to a JS object -}
exportSelectionsPaths : Model -> JE.Value
exportSelectionsPaths (DrawingArea model) =
    AnnSet.selectionsPathsObject model.annotations




-- OTHER #############################################################



{-| Indicates if the drawing area has at least one selection -}
hasSelection : Model -> Bool
hasSelection (DrawingArea model) =
    AnnSet.hasSelection model.annotations
