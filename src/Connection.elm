module Connection exposing (Connection, Socket(..), decoder, encode, getId, getIndex, removePreviousConnection, connectionHasNoNode)

import Json.Decode as Decode exposing (Decoder, int)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import List exposing (filter, any)
import Node exposing (Node)


type alias Connection =
    { input : Socket
    , output : Socket
    }


type Socket
    = Input Int Int
    | Output Int Int


getId : Socket -> Int
getId socket =
    case socket of
        Input id _ ->
            id

        Output id _ ->
            id


getIndex : Socket -> Int
getIndex socket =
    case socket of
        Input _ index ->
            index

        Output _ index ->
            index


encode : Connection -> Encode.Value
encode connection =
    Encode.object
        [ ( "input", encodeSocket connection.input )
        , ( "output", encodeSocket connection.output )
        ]


encodeSocket : Socket -> Encode.Value
encodeSocket socket =
    case socket of
        Input id index ->
            Encode.object
                [ ( "id", Encode.int id )
                , ( "index", Encode.int index )
                ]

        Output id index ->
            Encode.object
                [ ( "id", Encode.int id )
                , ( "index", Encode.int index )
                ]


decoder : Decoder Connection
decoder =
    Decode.succeed Connection
        |> required "input" inputDecoder
        |> required "output" outputDecoder


inputDecoder : Decoder Socket
inputDecoder =
    Decode.succeed Input
        |> required "id" int
        |> required "index" int


outputDecoder : Decoder Socket
outputDecoder =
    Decode.succeed Output
        |> required "id" int
        |> required "index" int


removePreviousConnection : List Connection -> Connection -> List Connection
removePreviousConnection connections newConnection =
    filter (\c -> c.input /= newConnection.input) connections


connectionHasNoNode : List Node -> Connection -> Bool
connectionHasNoNode nodes connection =
    not (connectionHasAnyNode nodes connection)


connectionHasAnyNode : List Node -> Connection -> Bool
connectionHasAnyNode nodes connection =
    any (connectionHasNode connection) nodes


connectionHasNode : Connection -> Node -> Bool
connectionHasNode connection node =
    getId connection.input == node.id || getId connection.output == node.id
