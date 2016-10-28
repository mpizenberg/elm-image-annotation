module DrawingArea exposing
    ( Tool(..), Model, init
    , Msg(..), update
    , view, viewLastOnly, viewWithImage, selectHtml, selectToolView
    , exportAnnotations, exportSelectionsPaths
    , hasSelection
    )

{-| The DrawingArea module aims at collecting annotations.

# Model
@docs Tool, Model, init

# Update
@docs Msg, update

# View
@docs view, viewLastOnly, viewWithImage, selectHtml, selectToolView

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


-- Options of the drawing area
type Option
    = Option_WheelZoom Bool


defaultOptions : List Option
defaultOptions =
    [ Option_WheelZoom False
    ]


type alias Model_ =
    { bgImage : Maybe Image.Model
    , annotations : AnnSet.Model
    , tool : Tool
    , options : List Option
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
        defaultOptions
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
    | Down Bool (Int, Int)
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
        Down createNewAnnotation (x, y) ->
            if createNewAnnotation
            then
                HP.updateFull
                    update
                    (DrawingArea model)
                    [CreateAnnotation, Down False (x,y)]
            else
                let
                    (ox, oy) = model.origin
                    x'' = round <| ox + (toFloat x) / model.zoomLevel
                    y'' = round <| oy + (toFloat y) / model.zoomLevel
                    model' = {model | downPos = Just (x'', y''), mouseDown = True}
                in
                    case model.tool of
                        NoTool -> ( DrawingArea model', Cmd.none )
                        RectangleTool ->
                            update
                                ( rsMsg <| RS.Geom (Just (x'', y'')) (Just (0,0)) )
                                ( DrawingArea model' )
                        OutlineTool ->
                            update
                                ( osMsg <| OS.ResetWithPoint (x'', y'') )
                                ( DrawingArea model' )
        Move (x', y') ->
            case model.tool of
                NoTool ->
                    let
                        (x,y) = model.origin
                    in
                        update
                            ( ChangeOrigin
                                ( x - toFloat x' / model.zoomLevel
                                , y - toFloat y' / model.zoomLevel
                                )
                            )
                            ( DrawingArea model )
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
                        update
                            ( rsMsg <| RS.Geom
                                (Just (left, top))
                                (Just (width, height))
                            )
                            ( DrawingArea model )
                OutlineTool ->
                    let
                        (ox, oy) = model.origin
                        x'' = round <| ox + (toFloat x') / model.zoomLevel
                        y'' = round <| oy + (toFloat y') / model.zoomLevel
                    in
                        update
                            ( osMsg <| OS.AddPoint (x'',y'') )
                            ( DrawingArea model )
        Up ->
            ( DrawingArea {model | mouseDown = False, downPos = Nothing}
            , Cmd.none
            )
        -- Background Image Management ->
        ChangeImage imModel ->
            let
                model' = {model | bgImage = imModel}
            in
                case imModel of
                    Nothing -> (DrawingArea model', Cmd.none)
                    Just im ->
                        update (CenterImage <| Image.size im) (DrawingArea model')
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
            update
                ( Annotations AnnSet.CreateAnnotation )
                ( DrawingArea model )
        DeleteAnnotation ->
            update
                ( Annotations AnnSet.Delete )
                ( DrawingArea model )
        ResetAnnotation ->
            update
                ( Annotations AnnSet.ResetAnnotation )
                ( DrawingArea model )
        SelectAnnotation maybeId ->
            update
                ( Annotations <| AnnSet.Select maybeId )
                ( DrawingArea model )
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
        update
            ( Center (centerX, centerY) )
            ( DrawingArea {model | zoomLevel = zoomModifier * model.zoomLevel} )


selMsg : Ann.SelectionMsg -> Msg
selMsg =
    Annotations << AnnSet.Annotate << Ann.Selection << Just


rsMsg : RS.Msg -> Msg
rsMsg = selMsg << Ann.RSMsg


osMsg : OS.Msg -> Msg
osMsg = selMsg << Ann.OSMsg




-- VIEW ##############################################################




{-| View the svg tag representing the DrawingArea model -}
view : Model -> Svg.Svg Msg
view (DrawingArea model) =
    Svg.svg
        ([ HE.onMouseUp Up
        , drawingAreaStyle model.size ]
        ++ offsetsEvents model.mouseDown False model.tool
        ++ optionsAttributes (DrawingArea model)
        )
        [ Svg.g
            [ svgTransform model.zoomLevel model.origin ]
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
        ]



{-| View with only the last annotation -}
viewLastOnly : Model -> Svg.Svg Msg
viewLastOnly (DrawingArea model) =
    Svg.svg
        ([ HE.onMouseUp Up
        , drawingAreaStyle model.size ]
        ++ offsetsEvents model.mouseDown True model.tool
        ++ optionsAttributes (DrawingArea model)
        )
        [ Svg.g
            [ svgTransform model.zoomLevel model.origin ]
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
            ++ AnnSet.selectionsViewLastOnly model.annotations
            )
        ]


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
        [ ("width", toString width ++ "px")
        , ("height", toString height ++ "px")
        ]


optionsAttributes : Model -> List (Svg.Attribute Msg)
optionsAttributes (DrawingArea model) =
    let
        optionToAttributes : Option -> List (Svg.Attribute Msg)
        optionToAttributes option = case option of
            Option_WheelZoom bool ->
                if bool then [ onWheel Wheel ] else []
    in
        List.concat <| List.map optionToAttributes model.options


offsetsEvents : Bool -> Bool -> Tool -> List (Svg.Attribute Msg)
offsetsEvents down createNewAnnotation tool =
    let
        baseOffsets = [(HP.offsetOn "mousedown") <| Down createNewAnnotation]
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
