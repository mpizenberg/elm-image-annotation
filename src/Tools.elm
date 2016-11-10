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


selectTag : Tool -> (Tool -> msg) -> Html msg
selectTag =
    HPV.autoSelectTag
        [ ( None, "None" )
        , ( Rectangle, "Rectangle" )
        , ( Outline, "Outline" )
        ]
