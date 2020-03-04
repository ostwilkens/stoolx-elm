port module Ports exposing (storeModel)

import Json.Encode as Encode


port storeModel : Encode.Value -> Cmd msg
