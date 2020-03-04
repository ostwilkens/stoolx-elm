port module Ports exposing (storeNodes)

import Json.Encode as Encode


port storeNodes : Encode.Value -> Cmd msg
