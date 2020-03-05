module Node exposing (Node, decoder, encode, init, inputCount, outputCount, previewCode, selected, setCode)

import Json.Decode as Decode exposing (Decoder, int, string)
import Json.Decode.Pipeline exposing (hardcoded, required)
import Json.Encode as Encode
import List exposing (head)
import String exposing (replace, split)
import Vec2 exposing (Vec2)


type alias Node =
    { pos : Vec2
    , selected : Bool
    , code : String
    , id : Int
    }


encode : Node -> Encode.Value
encode node =
    Encode.object
        [ ( "pos", Vec2.encode node.pos )
        , ( "code", Encode.string node.code )
        , ( "id", Encode.int node.id )
        ]


decoder : Decoder Node
decoder =
    Decode.succeed Node
        |> required "pos" Vec2.decoder
        |> hardcoded False
        |> required "code" string
        |> required "id" int


selected : Node -> Bool
selected node =
    node.selected


setCode : String -> Node -> Node
setCode code node =
    { node | code = code }


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


init : Int -> Node
init id =
    { pos = Vec2 200 300
    , selected = True
    , code = "x"
    , id = id
    }


previewCode : Node -> String
previewCode node =
    replace "float" "" node.code
        |> replace "vec2" ""
        |> replace "vec3" ""
        |> replace "vec4" ""
        |> replace "__0" ""
        |> replace "__1" ""
        |> replace "__2" ""
        |> replace "__3" ""
        |> replace "(" " "
        |> replace ")" " "
        |> replace "," " "
        |> replace "  " " "
        |> String.trim
