port module Port exposing (..)

import Json.Encode as E


port toggleDialog : String -> Cmd msg


port setStore : E.Value -> Cmd msg
