module Connection exposing (Connection, SocketRef, SocketType(..), decoder, encode)

import Json.Decode as Decode exposing (Decoder, field, int, map, oneOf, string)
import Json.Decode.Pipeline exposing (hardcoded, required)
import Json.Encode as Encode


type alias Connection =
    { input : SocketRef
    , output : SocketRef
    }


type SocketType
    = Input
    | Output


type alias SocketRef =
    { id : Int
    , index : Int
    , socketType : SocketType
    }


encode : Connection -> Encode.Value
encode connection =
    Encode.object
        [ ( "input", encodeSocketRef connection.input )
        , ( "output", encodeSocketRef connection.output )
        ]


decoder : Decoder Connection
decoder =
    Decode.succeed Connection
        |> required "input" (socketRefDecoder Input)
        |> required "output" (socketRefDecoder Output)


socketRefDecoder : SocketType -> Decoder SocketRef
socketRefDecoder socketType =
    Decode.succeed SocketRef
        |> required "id" int
        |> required "index" int
        |> required "socketType" (Decode.succeed socketType)


encodeSocketRef : SocketRef -> Encode.Value
encodeSocketRef socketRef =
    Encode.object
        [ ( "id", Encode.int socketRef.id )
        , ( "index", Encode.int socketRef.index )
        , ( "socketType", encodeSocketType socketRef.socketType )
        ]


encodeSocketType : SocketType -> Encode.Value
encodeSocketType socketType =
    case socketType of
        Input ->
            Encode.string "Input"

        Output ->
            Encode.string "Output"
