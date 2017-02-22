-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module Tool
    exposing
        ( Tool(..)
        , selectTag
        )

{-| Tools aims at providing helper functions related to the drawing area tools.

@docs Tool, selectTag
-}

import Html exposing (Html)
import Helpers.Select as Select


-- MODEL #############################################################


{-| The type of tool that can be used to draw inputs.
-}
type Tool
    = None
    | Rectangle
    | Outline
    | ScribbleFG
    | ScribbleBG



-- VIEW ##############################################################


{-| An html <select> tag enabling the choice of a tool.
-}
selectTag : (Tool -> msg) -> Tool -> Html msg
selectTag tagger current =
    Select.listTag
        tagger
        current
        ( None, "None" )
        [ ( Rectangle, "Rectangle" )
        , ( Outline, "Outline" )
        , ( ScribbleFG, "FG Scribble" )
        , ( ScribbleBG, "BG Scribble" )
        ]
