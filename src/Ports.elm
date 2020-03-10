port module Ports exposing (storeModel, setGlsl)

import Json.Encode as Encode


port storeModel : Encode.Value -> Cmd msg


port setGlsl : String -> Cmd msg
