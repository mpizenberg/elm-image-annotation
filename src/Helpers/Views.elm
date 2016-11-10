module Helpers.Views exposing (..)

import Html as H exposing (Html)
import Html.Attributes as HA
import Helpers.Events as HPE


-- SELECT ############################################################


type alias SelectConfig value =
    { describer : value -> String
    , encoder : value -> String
    , decoder : String -> value
    , allValues : List value
    }


selectTag : SelectConfig value -> value -> (value -> msg) -> Html msg
selectTag config currentValue msgMaker =
    H.select
        [ HPE.onChange <| msgMaker << config.decoder ]
        (List.map
            (optionTag config.encoder config.describer currentValue)
            config.allValues
        )


optionTag : (value -> String) -> (value -> String) -> value -> value -> Html msg
optionTag encoder describer currentValue value =
    H.option
        [ HA.value <| encoder value
        , HA.selected (currentValue == value)
        ]
        [ H.text <| describer value
        ]
