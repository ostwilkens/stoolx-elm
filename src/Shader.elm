module Shader exposing (fragmentShader)

import Model exposing (Model)
import List exposing (head, filter, map, member)
import String exposing (startsWith)
import List.Extra
import Socket exposing (Socket)
import Node exposing (Node)
import Connection exposing (Connection)


getCode : Model -> Node -> List String
getCode model node =
    let
        connections =
            filter (\c -> Socket.getId c.input == node.id) model.connections

        outputIds =
            map (\c -> Socket.getId c.output) connections

        inputNodes =
            filter (\n -> member n.id outputIds) model.nodes
    in
    getDeclarationString node connections model
        :: List.foldl (++) [] (map (getCode model) inputNodes)


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
                    getCode model colorNode

                code =
                    List.foldl (++) "" (List.Extra.unique declarations)
            in
            fragmentShaderPrepend ++ code ++ fragmentShaderAppend

        Nothing ->
            ""


fragmentShaderPrepend : String
fragmentShaderPrepend =
    """
precision mediump float;
uniform vec2 u_resolution;
uniform float u_time;
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
