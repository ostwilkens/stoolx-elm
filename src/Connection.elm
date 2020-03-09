module Connection exposing (Connection, connectionHasNoNode, decoder, encode, removePreviousConnection)

import Json.Decode as Decode exposing (Decoder, int)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import List exposing (any, filter)
import Node exposing (Node)
import Socket exposing (Socket(..))


type alias Connection =
    { input : Socket
    , output : Socket
    }


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
    Socket.getId connection.input == node.id || Socket.getId connection.output == node.id
