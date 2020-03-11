module Shader exposing (fragmentShader)

import Connection exposing (Connection)
import List exposing (filter, head, map, member)
import List.Extra
import Model exposing (Model)
import Node exposing (Node)
import Socket exposing (Socket)
import String exposing (startsWith)


getCode : Model -> Int -> Node -> List ( String, Int )
getCode model depth node =
    let
        connections =
            filter (\c -> Socket.getId c.input == node.id) model.connections

        outputIds =
            map (\c -> Socket.getId c.output) connections

        inputNodes =
            filter (\n -> member n.id outputIds) model.nodes
    in
    ( getDeclarationString node connections model, depth )
        :: List.foldl (++) [] (map (getCode model (depth + 1)) inputNodes)


getReplacementString : List Connection -> Model -> Int -> String
getReplacementString connections model index =
    case head (filter (\c -> Socket.getIndex c.input == index) connections) of
        Just connection ->
            getOutputReferenceString model connection.output

        Nothing ->
            "__" ++ String.fromInt index


getDeclarationString : Node -> List Connection -> Model -> String
getDeclarationString node connections model =
    let
        returnType =
            getReturnType node

        inputCount =
            Node.inputCount node

        placeholders =
            List.map (\n -> "__" ++ String.fromInt n) (List.range 0 (inputCount - 1))

        replacements =
            map (getReplacementString connections model) (List.range 0 (inputCount - 1))

        placeholdersReplacements =
            List.map2 (\a b -> ( a, b )) placeholders replacements

        code =
            List.foldl (\( p, r ) c -> String.replace p r c) node.code placeholdersReplacements
    in
    if startsWith "!" code then
        String.dropLeft 1 code ++ ";"

    else
        returnType ++ " _" ++ String.fromInt node.id ++ " = " ++ code ++ ";\u{000D}\n"


getReturnType : Node -> String
getReturnType node =
    case head (String.split "(" node.code) of
        Just returnType ->
            returnType

        Nothing ->
            "?"


getOutputReferenceString : Model -> Socket -> String
getOutputReferenceString model output =
    let
        maybeOutputNode =
            Node.getById (Socket.getId output) model.nodes
    in
    case maybeOutputNode of
        Just outputNode ->
            let
                returnType =
                    getReturnType outputNode
            in
            if returnType == "float" then
                "_" ++ String.fromInt outputNode.id

            else
                case Socket.getIndex output of
                    0 ->
                        "_" ++ String.fromInt outputNode.id ++ ".x"

                    1 ->
                        "_" ++ String.fromInt outputNode.id ++ ".y"

                    2 ->
                        "_" ++ String.fromInt outputNode.id ++ ".z"

                    3 ->
                        "_" ++ String.fromInt outputNode.id ++ ".w"

                    _ ->
                        "?"

        Nothing ->
            "?"


fragmentShader : Model -> String
fragmentShader model =
    let
        maybeColorNode =
            head (filter (\n -> startsWith "!color" n.code) model.nodes)
    in
    case maybeColorNode of
        Just colorNode ->
            let
                declarations =
                    getCode model 0 colorNode

                sortedDeclarations =
                    List.sortWith (\( _, ad ) ( _, bd ) -> compare bd ad) declarations

                declarations2 =
                    map (\( a, _ ) -> a) sortedDeclarations

                declarations3 =
                    List.Extra.unique declarations2

                declarations4 =
                    List.reverse declarations3

                code =
                    List.foldl (++) "" (List.Extra.unique declarations4)
            in
            fragmentShaderPrepend ++ code ++ fragmentShaderAppend

        Nothing ->
            ""



-- float _4 = float(u_time);
-- float _3 = float(sin(_7.x));
-- vec3 _8 = vec3(rotXY(vec3(_3, _4, _7.x), _3));
-- vec2 _7 = vec2(gl_FragCoord.xy) / 100.0;
-- vec2 _5 = vec2(u_resolution) / 1000.0;
-- float _6 = float(length(vec2(_5.y, _5.x)));
-- color = vec3(_8.x, _7.y, _6);;


fragmentShaderPrepend : String
fragmentShaderPrepend =
    """
precision mediump float;
uniform vec2 u_resolution;
uniform float u_time;

// thx to hg (http://mercury.sexy/hg_sdf)
vec2 pR(vec2 p, float a) {
\treturn cos(a)*p + sin(a)*vec2(p.y, -p.x);
}

vec3 rotXY(vec3 p, float a) {
    vec2 xy = pR(p.xy, a);
    return vec3(xy, p.z);
}

vec3 rotYZ(vec3 p, float a) {
    vec2 yz = pR(p.yz, a);
    return vec3(p.x, yz);
}

vec3 rotZX(vec3 p, float a) {
    vec2 zx = pR(p.zx, a);
    return vec3(zx.x, p.y, zx.y);
}

void main(){
//u_time
vec2 st = gl_FragCoord.xy/u_resolution.xy;
vec3 color = vec3(0.0);
"""


fragmentShaderAppend : String
fragmentShaderAppend =
    """
gl_FragColor = vec4(color, 1.0);
}
"""
