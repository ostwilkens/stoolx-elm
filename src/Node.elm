module Node exposing (Node, decoder, encode, inputCount, outputCount, setCode)

import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (hardcoded, required)
import Json.Encode as Encode
import List exposing (head)
import String exposing (split)
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


setCode : String -> Node -> Node
setCode code node =
    if node.selected then
        { node | code = code }

    else
        node


returnType : Node -> Maybe String
returnType node =
    head (split "(" node.code)


outputCount : Node -> Int
outputCount node =
    case Maybe.withDefault "" (returnType node) of
        "float" ->
            1

        "vec2" ->
            2

        "vec3" ->
            3

        "vec4" ->
            4

        _ ->
            0


inputCount : Node -> Int
inputCount node =
    List.length (split "__" node.code) - 1
