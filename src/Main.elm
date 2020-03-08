module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Canvas
import Canvas.Settings
import Canvas.Settings.Line
import Color
import Connection exposing (Connection, Socket(..), connectionHasNoNode, decoder, getId, getIndex, removePreviousConnection)
import Element exposing (Attribute, Color, Element, alignBottom, alignRight, alignTop, centerX, centerY, column, el, fill, height, htmlAttribute, inFront, moveDown, moveRight, padding, paddingXY, px, rgb, rgba, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes
import Html.Events.Extra.Mouse as Mouse
import Json.Encode as Encode
import List exposing (any, filter, head, map, maximum, range)
import Model exposing (Model, Msg(..), SavedModel, connecting, decodeStoredModel, removeSelected)
import Node exposing (Node, decoder, encode, inputCount, outputCount, previewCode, setCode)
import Ports exposing (storeModel)
import Shader exposing (fragmentShader, mesh, vertexShader)
import Task
import Vec2 exposing (Vec2, encode)
import WebGL


init : Maybe Encode.Value -> ( Model, Cmd Msg )
init flags =
    let
        savedModel =
            case flags of
                Just modelJson ->
                    decodeStoredModel modelJson

                Nothing ->
                    { nodes = [], connections = [] }
    in
    ( { nodes = savedModel.nodes
      , connections = savedModel.connections
      , dragging = False
      , lastCursorPos = Vec2.zero
      , time = 0
      , windowSize = Vec2.zero
      , connectingSocket = Nothing
      }
    , Task.perform InitWindowSize Browser.Dom.getViewport
    )


main : Program (Maybe Encode.Value) Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta UpdateTime
        , Browser.Events.onResize (\w h -> ResizeWindow (Vec2 (toFloat w) (toFloat h)))
        ]


nextId : Model -> Int
nextId model =
    Maybe.withDefault 0 (maximum (map (\n -> n.id) model.nodes)) + 1


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Select node ->
            let
                startDragging =
                    not (connecting model)
            in
            ( { model
                | nodes = map (select node) model.nodes
                , dragging = startDragging
              }
            , Cmd.none
            )

        Deselect ->
            ( { model | nodes = map deselect model.nodes }
            , Cmd.none
            )

        Release ->
            ( { model
                | dragging = False
                , connectingSocket = Nothing
              }
            , Cmd.none
            )

        Drag pos ->
            if model.dragging then
                let
                    delta =
                        Vec2.sub pos model.lastCursorPos
                in
                ( { model
                    | nodes = map (drag delta) model.nodes
                    , lastCursorPos = pos
                  }
                , Cmd.none
                )

            else
                ( { model | lastCursorPos = pos }
                , Cmd.none
                )

        Add ->
            let
                id =
                    nextId model
            in
            ( { model | nodes = Node.init id :: map deselect model.nodes }
            , Cmd.none
            )

        Remove ->
            ( removeSelected model
            , Cmd.none
            )

        Save ->
            ( model
            , saveModel model
            )

        SetCode code ->
            ( { model | nodes = map (setCode code) model.nodes }
            , Cmd.none
            )

        UpdateTime delta ->
            ( { model | time = model.time + delta }
            , Cmd.none
            )

        StartConnect socket ->
            ( { model | connectingSocket = Just socket }
            , Cmd.none
            )

        ResizeWindow vec2 ->
            ( { model | windowSize = vec2 }
            , Cmd.none
            )

        InitWindowSize viewport ->
            ( { model | windowSize = Vec2 viewport.viewport.width viewport.viewport.height }
            , Cmd.none
            )

        Connect socket ->
            case model.connectingSocket of
                Just otherSocket ->
                    let
                        connection =
                            connectSockets model socket otherSocket
                    in
                    case connection of
                        Just justConnection ->
                            ( { model | connections = justConnection :: removePreviousConnection model.connections justConnection }
                            , Cmd.none
                            )

                        Nothing ->
                            ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )


saveModel : Model -> Cmd msg
saveModel model =
    encodeModel model
        |> Ports.storeModel


encodeModel : Model -> Encode.Value
encodeModel model =
    Encode.object
        [ ( "nodes", Encode.list Node.encode model.nodes )
        , ( "connections", Encode.list Connection.encode model.connections )
        ]


select : Node -> Node -> Node
select target node =
    { node | selected = node == target }


deselect : Node -> Node
deselect node =
    { node | selected = False }


connectSockets : Model -> Socket -> Socket -> Maybe Connection
connectSockets model a b =
    let
        input =
            case a of
                Input _ _ ->
                    a

                Output _ _ ->
                    b

        output =
            case a of
                Output _ _ ->
                    a

                Input _ _ ->
                    b

        inputExists =
            any (\n -> n.id == getId input) model.nodes

        inputIsInput =
            case input of
                Input _ _ ->
                    True

                Output _ _ ->
                    False

        outputExists =
            any (\n -> n.id == getId output) model.nodes

        outputIsOutput =
            case output of
                Output _ _ ->
                    True

                Input _ _ ->
                    False

        wouldBeDuplicate =
            any (\c -> c.input == input && c.output == output) model.connections

        selfReference =
            getId input == getId output

        valid =
            inputExists
                && inputIsInput
                && outputExists
                && outputIsOutput
                && not wouldBeDuplicate
                && not selfReference
    in
    if valid then
        Just { input = input, output = output }

    else
        Nothing


drag : Vec2 -> Node -> Node
drag offset node =
    if node.selected then
        { node | pos = Vec2.add node.pos offset }

    else
        node


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


clientPos : Mouse.Event -> Vec2
clientPos event =
    let
        ( x, y ) =
            event.clientPos
    in
    Vec2 x y


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


view : Model -> Html Msg
view model =
    Element.layout [] <|
        row [ height fill, width fill ]
            [ el
                ([ width fill
                 , height fill
                 , htmlAttribute (Mouse.onMove (clientPos >> Drag))
                 , Events.onMouseUp Release
                 , Events.onDoubleClick Deselect
                 , Element.behindContent (shaderEl model.time)
                 , inFront (codeEl model.nodes)
                 ]
                    ++ map inFront (map nodeEl model.nodes)
                    ++ [ inFront (canvasEl model) ]
                )
                menuEl
            , Element.none
            ]


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


nodeEl : Node -> Element Msg
nodeEl node =
    el
        [ moveRight node.pos.x
        , moveDown node.pos.y
        ]
        (column
            [ Background.color gray
            , nodeBorderColor node
            , nodeBorderWidth node
            , width (px 100)
            , height (px 100)
            , spacing 20
            , Events.onMouseDown (Select node)
            , Font.size 10
            ]
            [ outputsEl node.id (outputCount node)
            , codePreviewEl node
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


shaderEl : Float -> Element Msg
shaderEl time =
    Element.html
        (WebGL.toHtml
            [ Html.Attributes.style "height" "100%"
            , Html.Attributes.style "width" "100%"
            ]
            [ WebGL.entity vertexShader fragmentShader mesh { time = time / 1000 }
            ]
        )


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
            ]
            ([ Canvas.clear ( 0, 0 ) width height
             , connectingLine model
             ]
                ++ connectedLines model
            )
        )


socketIndexOffsetX : Int -> Int -> Float
socketIndexOffsetX index count =
    toFloat (50 + 15 + index * 30 - count * 15)


socketTypeOffsetY : Socket -> Float
socketTypeOffsetY socket =
    case socket of
        Input _ _ ->
            87

        Output _ _ ->
            13


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
                    socketIndexOffsetX (getIndex socket) count

                offsetY =
                    socketTypeOffsetY socket
            in
            Vec2 (justNode.pos.x + offsetX) (justNode.pos.y + offsetY)

        Nothing ->
            Vec2 0 0


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
