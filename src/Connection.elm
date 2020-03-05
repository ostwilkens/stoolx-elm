module Connection exposing (Connection, Socket, SocketType(..), decoder, encode)

import Json.Decode as Decode exposing (Decoder, int, string)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode


type alias Connection =
    { input : Socket
    , output : Socket
    }


type SocketType
    = Input
    | Output


type alias Socket =
    { id : Int
    , index : Int
    , socketType : SocketType
    }


encode : Connection -> Encode.Value
encode connection =
    Encode.object
        [ ( "input", encodeSocket connection.input )
        , ( "output", encodeSocket connection.output )
        ]


decoder : Decoder Connection
decoder =
    Decode.succeed Connection
        |> required "input" (socketDecoder Input)
        |> required "output" (socketDecoder Output)


socketDecoder : SocketType -> Decoder Socket
socketDecoder socketType =
    Decode.succeed Socket
        |> required "id" int
        |> required "index" int
        |> required "socketType" (Decode.succeed socketType)


encodeSocket : Socket -> Encode.Value
encodeSocket socket =
    Encode.object
        [ ( "id", Encode.int socket.id )
        , ( "index", Encode.int socket.index )
        , ( "socketType", encodeSocketType socket.socketType )
        ]


encodeSocketType : SocketType -> Encode.Value
encodeSocketType socketType =
    case socketType of
        Input ->
            Encode.string "Input"

        Output ->
            Encode.string "Output"
