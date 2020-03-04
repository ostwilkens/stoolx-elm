module Node exposing (Node, decoder, encode)

import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (hardcoded, required)
import Json.Encode as Encode
import Vec2 exposing (Vec2)


type alias Node =
    { pos : Vec2
    , selected : Bool
    , code : String
    }


encode : Node -> Encode.Value
encode node =
    Encode.object
        [ ( "pos", Vec2.encode node.pos )
        , ( "code", Encode.string node.code )
        ]


decoder : Decoder Node
decoder =
    Decode.succeed Node
        |> required "pos" Vec2.decoder
        |> hardcoded False
        |> required "code" string
