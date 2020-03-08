module Vec2 exposing (Vec2, add, decoder, encode, sub, zero)

import Json.Decode as Decode exposing (Decoder, float)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode


type alias Vec2 =
    { x : Float
    , y : Float
    }


add : Vec2 -> Vec2 -> Vec2
add a b =
    Vec2 (a.x + b.x) (a.y + b.y)


sub : Vec2 -> Vec2 -> Vec2
sub a b =
    Vec2 (a.x - b.x) (a.y - b.y)


decoder : Decoder Vec2
decoder =
    Decode.succeed Vec2
        |> required "x" float
        |> required "y" float


encode : Vec2 -> Encode.Value
encode vec2 =
    Encode.object
        [ ( "x", Encode.float vec2.x )
        , ( "y", Encode.float vec2.y )
        ]

zero : Vec2
zero =
    Vec2 0 0 