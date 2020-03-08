module Node exposing (Node, decoder, deselect, encode, getSelectedCode, height, init, inputCount, move, nextId, outputCount, previewCode, select, selected, setCode, width)

import Json.Decode as Decode exposing (Decoder, int, string)
import Json.Decode.Pipeline exposing (hardcoded, required)
import Json.Encode as Encode
import List exposing (filter, head, map, maximum)
import String exposing (replace, split)
import Vec2 exposing (Vec2)


type alias Node =
    { pos : Vec2
    , selected : Bool
    , code : String
    , id : Int
    }


width : Int
width =
    100


height : Int
height =
    100


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


getSelectedCode : List Node -> String
getSelectedCode nodes =
    let
        selectedNodes =
            filter (\n -> n.selected) nodes
    in
    case head selectedNodes of
        Just node ->
            node.code

        Nothing ->
            ""


nextId : List Node -> Int
nextId nodes =
    Maybe.withDefault 0 (maximum (map (\n -> n.id) nodes)) + 1


select : Node -> Node -> Node
select target node =
    { node | selected = node == target }


deselect : Node -> Node
deselect node =
    { node | selected = False }


move : Vec2 -> Node -> Node
move offset node =
    if node.selected then
        { node | pos = Vec2.add node.pos offset }

    else
        node
