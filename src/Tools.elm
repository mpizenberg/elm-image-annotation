module Tools exposing (..)

import Html as H exposing (Html)
import Html.Attributes as HA
import Helpers.Events as HPE
import Helpers.Views as HPV


-- MODEL #############################################################


{-| The type of tool that can be used to draw selections.
-}
type Tool
    = None
    | Rectangle
    | Outline



-- VIEW ##############################################################


selectConfig : HPV.SelectConfig Tool
selectConfig =
    { describer = toolName
    , encoder = toolName
    , decoder = stringToTool
    , allValues = [ None, Rectangle, Outline ]
    }


{-| A select form tag to change dynamically the current Tool
-}
selectTag : Tool -> (Tool -> msg) -> Html msg
selectTag =
    HPV.selectTag selectConfig


stringToTool : String -> Tool
stringToTool toolName =
    case toolName of
        "Rectangle" ->
            Rectangle

        "Outline" ->
            Outline

        _ ->
            None


toolName : Tool -> String
toolName tool =
    case tool of
        None ->
            "None"

        Rectangle ->
            "Rectangle"

        Outline ->
            "Outline"
