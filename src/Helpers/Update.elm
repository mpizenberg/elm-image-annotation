module Helpers.Update exposing (updateFull)

{-| Helpers to fully update a model
-}


{-| Update again a couple (model, cmd) with a new message
-}
updateAgain :
    (msg -> model -> ( model, Cmd msg ))
    -> msg
    -> ( model, Cmd msg )
    -> ( model, Cmd msg )
updateAgain updateFunction msg ( model, cmd ) =
    let
        ( model', cmd' ) =
            updateFunction msg model
    in
        model' ! [ cmd, cmd' ]


{-| Update with multiple messages in order
-}
updateFull :
    (msg -> model -> ( model, Cmd msg ))
    -> model
    -> List msg
    -> ( model, Cmd msg )
updateFull updateFunction model =
    List.foldl
        (updateAgain updateFunction)
        ( model, Cmd.none )
