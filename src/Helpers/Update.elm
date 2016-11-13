-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


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
