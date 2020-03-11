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
            head (filter (\n -> startsWith "!" n.code) model.nodes)
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

float scene(vec3 p)
{
    // vec3 field = max(min(pos, vec3(0.1)), vec3(-0.1));
"""


fragmentShaderAppend : String
fragmentShaderAppend =
    """
    return field;
}

void main()
{
//u_time
vec3 cameraOrigin = vec3(0., -0.4, 10.0);
vec3 cameraTarget = cameraOrigin + vec3(.0, -1., -10);
vec3 upDirection = vec3(0., 1.0, 0.);
vec3 cameraDir = normalize(cameraTarget - cameraOrigin);
vec3 cameraRight = normalize(cross(upDirection, cameraOrigin));
vec3 cameraUp = cross(cameraDir, cameraRight);

vec2 uv = -1.0 + 2.0 * gl_FragCoord.xy / u_resolution.xy;
uv.x *= u_resolution.x / u_resolution.y;

vec3 rayDir = normalize(cameraRight * uv.x + cameraUp * uv.y + cameraDir);

const float MAX_DIST = 100.0;
const float EPSILON = 0.1;

float totalDist = 0.0;
vec3 p = cameraOrigin;
float dist = EPSILON;

for(int i = 0; i < 10; i++)
{
    if (dist < EPSILON || totalDist > MAX_DIST)
        break;

    dist = scene(p);
    totalDist += dist;
    p += dist * rayDir;
}

vec2 eps = vec2(0.0, EPSILON);
vec3 normal = normalize(vec3(
    scene(p + eps.yxx) - scene(p - eps.yxx),
    scene(p + eps.xyx) - scene(p - eps.xyx),
    scene(p + eps.xxy) - scene(p - eps.xxy)));

float diffuse = max(.0, dot(-rayDir, normal));
float specular = pow(diffuse, 10.0);

vec3 c = vec3(0.);
c += smoothstep(0., 1.2, diffuse + 0.05) * 0.85;
c += smoothstep(0., 1., specular) * 0.1;
c = sqrt(c - 0.1) * 1.05;

gl_FragColor = vec4(c, 1.0);
}
"""
