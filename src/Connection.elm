module Connection exposing (Connection, Socket(..), decoder, encode, getId, getIndex)

import Json.Decode as Decode exposing (Decoder, int)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode


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
