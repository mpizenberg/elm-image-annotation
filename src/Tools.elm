-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module Tools
    exposing
        ( Tool(..)
        , selectTag
        )

{-| Tools aims at providing helper functions related to the drawing area tools.

@docs Tool, selectTag
-}

import Html as H exposing (Html)
import Helpers.Views as HPV


-- MODEL #############################################################


{-| The type of tool that can be used to draw selections.
-}
type Tool
    = None
    | Rectangle
    | Outline



-- VIEW ##############################################################


{-| An html <select> tag enabling the choice of a tool.
-}
selectTag : Tool -> (Tool -> msg) -> Html msg
selectTag =
    HPV.autoSelectTag
        [ ( None, "None" )
        , ( Rectangle, "Rectangle" )
        , ( Outline, "Outline" )
        ]
