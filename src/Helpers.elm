module Helpers exposing
    (..)

import Task


{-| Transform a message to a Cmd message -}
msgToCmd : msg -> Cmd msg
msgToCmd message =
    Task.perform identity identity (Task.succeed message)
