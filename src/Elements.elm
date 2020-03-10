module Elements exposing (canvasEl, codeEl, codeFont, menuEl, nodeEl, shaderEl, white)

import Canvas
import Canvas.Settings
import Canvas.Settings.Line
import Color
import Connection exposing (Connection)
import Element exposing (Attribute, Color, Element, alignBottom, alignRight, alignTop, centerX, centerY, column, el, fill, height, moveDown, moveRight, paddingXY, px, rgb, rgba, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html
import Html.Attributes
import List exposing (any, filter, head, map, member, range)
import List.Extra
import Model exposing (Model, Msg(..))
import Node exposing (Node, getSelectedCode, inputCount, outputCount, previewCode)
import Socket exposing (Socket(..), getId, getIndex)
import String exposing (startsWith)
import Vec2 exposing (Vec2)


nodeEl : Vec2 -> Node -> Element Msg
nodeEl center node =
    el
        [ moveRight (node.pos.x + center.x - (toFloat Node.width / 2))
        , moveDown (node.pos.y + center.y - (toFloat Node.height / 2))
        ]
        (column
            [ Background.color gray
            , nodeBorderColor node
            , nodeBorderWidth node
            , width (px Node.width)
            , height (px Node.height)
            , spacing 20
            , Events.onMouseDown (Select node)
            , Font.size 10
            , Element.behindContent (codePreviewEl node)
            ]
            [ outputsEl node.id (outputCount node)
            , inputsEl node.id (inputCount node)
            ]
        )


inputsEl : Int -> Int -> Element Msg
inputsEl id count =
    row [ alignBottom, spacing 10, centerX ]
        (map (\i -> socketEl (Input id i)) (range 0 (count - 1)))


outputsEl : Int -> Int -> Element Msg
outputsEl id count =
    row [ alignTop, spacing 10, centerX ]
        (map (\i -> socketEl (Output id i)) (range 0 (count - 1)))


socketEl : Socket -> Element Msg
socketEl socket =
    el
        [ width (px 20)
        , height (px 20)
        , Background.color (rgb 0.9 0.3 0.3)
        , Events.onMouseDown (StartConnect socket)
        , Events.onMouseUp (Connect socket)
        ]
        Element.none


shaderEl : Model -> Element Msg
shaderEl model =
    Element.html
        (Html.canvas
            [ Html.Attributes.style "background" "black"
            , Html.Attributes.style "height" "100%"
            , Html.Attributes.attribute "class" "glslCanvas"
            , Html.Attributes.attribute "data-fragment" (fragmentShader model)
            ]
            []
        )


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


canvasEl : Model -> Element Msg
canvasEl model =
    let
        width =
            model.windowSize.x

        height =
            model.windowSize.y
    in
    Element.html
        (Canvas.toHtml ( floor width, floor height )
            [ Html.Attributes.style "pointer-events" "none"
            , Html.Attributes.style "overflow" "hidden"
            ]
            ([ Canvas.clear ( 0, 0 ) width height
             , connectingLine model
             ]
                ++ connectedLines model
            )
        )


connectingLine : Model -> Canvas.Renderable
connectingLine model =
    case model.connectingSocket of
        Just socket ->
            line (socketPos model socket) model.lastCursorPos

        Nothing ->
            line Vec2.zero Vec2.zero


connectedLine : Model -> Connection -> Canvas.Renderable
connectedLine model connection =
    line (socketPos model connection.input) (socketPos model connection.output)


connectedLines : Model -> List Canvas.Renderable
connectedLines model =
    map (connectedLine model) model.connections


line : Vec2 -> Vec2 -> Canvas.Renderable
line a b =
    Canvas.shapes
        [ Canvas.Settings.stroke (Color.rgba 0 1 0 0.7)
        , Canvas.Settings.Line.lineWidth 5
        ]
        [ Canvas.path ( a.x, a.y )
            [ Canvas.lineTo ( b.x, b.y )
            ]
        ]


socketPos : Model -> Socket -> Vec2
socketPos model socket =
    let
        node =
            head (filter (\n -> n.id == getId socket) model.nodes)
    in
    case node of
        Just justNode ->
            let
                count =
                    case socket of
                        Output _ _ ->
                            outputCount justNode

                        Input _ _ ->
                            inputCount justNode

                offsetX =
                    socketIndexOffsetX (getIndex socket) count + model.center.x - toFloat Node.width / 2

                offsetY =
                    socketTypeOffsetY socket + model.center.y - toFloat Node.height / 2
            in
            Vec2 (justNode.pos.x + offsetX) (justNode.pos.y + offsetY)

        Nothing ->
            Vec2 0 0


menuEl : Element Msg
menuEl =
    row
        [ alignBottom
        , centerX
        , spacing 5
        , Font.family narrowFont
        , Font.size 18
        , Font.color black
        ]
        [ addButton
        , removeButton
        , saveButton
        ]


codePreviewEl : Node -> Element Msg
codePreviewEl node =
    el
        [ centerX
        , centerY
        , Font.size 12
        , Font.color white
        , Font.family narrowFont
        ]
        (text (previewCode node))


nodeBorderWidth : Node -> Attribute Msg
nodeBorderWidth node =
    if node.selected then
        Border.width 3

    else
        Border.width 1


nodeBorderColor : Node -> Attribute Msg
nodeBorderColor node =
    if node.selected then
        Border.color red

    else
        Border.color black


socketIndexOffsetX : Int -> Int -> Float
socketIndexOffsetX index count =
    toFloat (floor (toFloat Node.width / 2) + 15 + index * 30 - count * 15)


socketTypeOffsetY : Socket -> Float
socketTypeOffsetY socket =
    case socket of
        Input _ _ ->
            toFloat Node.height - 13

        Output _ _ ->
            13


red : Color
red =
    rgb 0.8 0.1 0.1


gray : Color
gray =
    rgb 0.3 0.3 0.3


darkGray : Color
darkGray =
    rgb 0.15 0.15 0.15


lightGray : Color
lightGray =
    rgb 0.43 0.43 0.43


black : Color
black =
    rgb 0 0 0


white : Color
white =
    rgb 1 1 1


addButton : Element Msg
addButton =
    Input.button
        [ Background.color red
        , paddingXY 10 6
        ]
        { label = text "add"
        , onPress = Just Add
        }


removeButton : Element Msg
removeButton =
    Input.button
        [ Background.color red
        , paddingXY 10 6
        ]
        { label = text "remove"
        , onPress = Just Remove
        }


saveButton : Element Msg
saveButton =
    Input.button
        [ Background.color red
        , paddingXY 10 6
        ]
        { label = text "save"
        , onPress = Just Save
        }


codeFont : List Font.Font
codeFont =
    [ Font.typeface "Courier New"
    , Font.sansSerif
    ]


narrowFont : List Font.Font
narrowFont =
    [ Font.typeface "Arial Narrow"
    , Font.sansSerif
    ]


codeEl : List Node -> Element Msg
codeEl nodes =
    if any (\n -> n.selected) nodes then
        Input.multiline
            [ width fill
            , height (px 100)
            , Background.color (rgba 0.3 0.3 0.3 0.5)
            , Font.color white
            , Font.size 14
            , Font.family codeFont
            , alignRight
            ]
            { label = Input.labelHidden "code"
            , onChange = SetCode
            , placeholder = Nothing
            , text = getSelectedCode nodes
            , spellcheck = False
            }

    else
        Element.none
