module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Canvas
import Canvas.Settings
import Canvas.Settings.Line
import Color
import Element exposing (Attribute, Color, Element, alignBottom, alignRight, alignTop, centerX, centerY, column, el, fill, height, htmlAttribute, inFront, moveDown, moveRight, padding, paddingXY, px, rgb, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes
import Html.Events.Extra.Mouse as Mouse
import Json.Decode as Decode
import Json.Encode as Encode
import List exposing (any, filter, head, map, maximum, range, repeat)
import Node exposing (Node, decoder, encode, inputCount, outputCount, previewCode, setCode)
import Ports exposing (storeNodes)
import Shader exposing (fragmentShader, mesh, vertexShader)
import Task
import Vec2 exposing (Vec2, encode)
import WebGL


type alias Model =
    { nodes : List Node
    , dragging : Bool
    , lastCursorPos : Vec2
    , time : Float
    , connecting : Bool
    , size : ( Float, Float )
    , connectingSocketRef : Maybe SocketRef
    , connections : List Connection
    }


init : Maybe Encode.Value -> ( Model, Cmd Msg )
init flags =
    let
        nodes =
            case flags of
                Just nodesJson ->
                    decodeStoredNodes nodesJson

                Nothing ->
                    []
    in
    ( { nodes = nodes
      , dragging = False
      , lastCursorPos = Vec2 0 0
      , time = 0
      , connecting = False
      , size = ( 0, 0 )
      , connectingSocketRef = Nothing
      , connections = []
      }
    , Task.perform InitSize Browser.Dom.getViewport
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
        , Browser.Events.onResize (\w h -> Resize ( toFloat w, toFloat h ))
        ]


type Msg
    = Select Node
    | Deselect
    | Release
    | Drag Vec2
    | Add
    | Remove
    | Save
    | SetCode String
    | UpdateTime Float
    | StartConnect SocketRef
    | Resize ( Float, Float )
    | InitSize Browser.Dom.Viewport
    | EndConnect SocketRef


nextId : Model -> Int
nextId model =
    Maybe.withDefault 0 (maximum (map (\n -> n.id) model.nodes)) + 1


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Select node ->
            let
                startDragging =
                    not model.connecting
            in
            ( { model | nodes = map (select node) model.nodes, dragging = startDragging }
            , Cmd.none
            )

        Deselect ->
            ( { model | nodes = map deselect model.nodes }
            , Cmd.none
            )

        Release ->
            ( { model | dragging = False, connecting = False }
            , Cmd.none
            )

        Drag pos ->
            if model.dragging then
                ( { model | nodes = map (drag (Vec2.sub pos model.lastCursorPos)) model.nodes, lastCursorPos = pos }
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
            ( { model | nodes = filter (\n -> not n.selected) model.nodes }
            , Cmd.none
            )

        Save ->
            ( model
            , saveNodes model.nodes
            )

        SetCode code ->
            ( { model | nodes = map (setCode code) model.nodes }
            , Cmd.none
            )

        UpdateTime delta ->
            ( { model | time = model.time + delta }
            , Cmd.none
            )

        StartConnect socketRef ->
            ( { model | connecting = True, connectingSocketRef = Just socketRef }
            , Cmd.none
            )

        Resize ( w, h ) ->
            ( { model | size = ( w, h ) }
            , Cmd.none
            )

        InitSize viewport ->
            ( { model | size = ( viewport.viewport.width, viewport.viewport.height ) }
            , Cmd.none
            )

        EndConnect socketRef ->
            case model.connectingSocketRef of
                Just justConnectingSocketRef ->
                    let
                        connection =
                            connectSockets justConnectingSocketRef socketRef
                    in
                    case connection of
                        Just justConnection ->
                            ( { model | connections = justConnection :: model.connections }
                            , Cmd.none
                            )

                        Nothing ->
                            ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )


connectSockets : SocketRef -> SocketRef -> Maybe Connection
connectSockets a b =
    let
        input =
            if a.socketType == Input then
                a

            else
                b

        output =
            if a.socketType == Output then
                a

            else
                b

        valid =
            input.socketType == Input && output.socketType == Output
    in
    if valid then
        Just { input = input, output = output }

    else
        Nothing


saveNodes : List Node -> Cmd msg
saveNodes nodes =
    Encode.list Node.encode nodes
        |> Ports.storeNodes


decodeStoredNodes : Encode.Value -> List Node
decodeStoredNodes nodesJson =
    case Decode.decodeValue (Decode.list Node.decoder) nodesJson of
        Ok nodes ->
            nodes

        Err _ ->
            []


select : Node -> Node -> Node
select target node =
    { node | selected = node == target }


deselect : Node -> Node
deselect node =
    { node | selected = False }


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


codeInput : List Node -> Element Msg
codeInput nodes =
    if any (\n -> n.selected) nodes then
        Input.multiline
            [ height fill
            , width (px 300)
            , Background.color gray
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
                 , inFront (codeInput model.nodes)
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
            [ putsEl node.id Output (outputCount node)
            , codePreviewEl node
            , putsEl node.id Input (inputCount node)
            ]
        )


putsEl : Int -> SocketType -> Int -> Element Msg
putsEl id socketType count =
    let
        alignment =
            if socketType == Output then
                alignTop

            else
                alignBottom
    in
    row [ alignment, spacing 10, centerX ]
        (map (\i -> putEl { id = id, index = i, socketType = socketType }) (range 0 (count - 1)))


putEl : SocketRef -> Element Msg
putEl socketRef =
    el
        [ width (px 20)
        , height (px 20)
        , Background.color (rgb 0.9 0.3 0.3)
        , Events.onMouseDown (StartConnect socketRef)
        , Events.onMouseUp (EndConnect socketRef)
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
            Tuple.first model.size

        height =
            Tuple.second model.size
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


socketTypeOffsetY : SocketType -> Float
socketTypeOffsetY socketType =
    case socketType of
        Output ->
            13

        Input ->
            87


socketRefPos : Model -> SocketRef -> ( Float, Float )
socketRefPos model socketRef =
    let
        node =
            head (filter (\n -> n.id == socketRef.id) model.nodes)
    in
    case node of
        Just justNode ->
            let
                count =
                    case socketRef.socketType of
                        Output ->
                            outputCount justNode

                        Input ->
                            inputCount justNode

                offsetX =
                    socketIndexOffsetX socketRef.index count

                offsetY =
                    socketTypeOffsetY socketRef.socketType
            in
            ( justNode.pos.x + offsetX, justNode.pos.y + offsetY )

        Nothing ->
            ( 0, 0 )


connectingLine : Model -> Canvas.Renderable
connectingLine model =
    if model.connecting then
        let
            a =
                case model.connectingSocketRef of
                    Just socketRef ->
                        socketRefPos model socketRef

                    Nothing ->
                        ( 100, 0 )

            b =
                ( model.lastCursorPos.x, model.lastCursorPos.y )
        in
        line a b

    else
        line ( 0, 0 ) ( 0, 0 )


connectedLine : Model -> Connection -> Canvas.Renderable
connectedLine model connection =
    line (socketRefPos model connection.input) (socketRefPos model connection.output)


connectedLines : Model -> List Canvas.Renderable
connectedLines model =
    map (connectedLine model) model.connections


line : ( Float, Float ) -> ( Float, Float ) -> Canvas.Renderable
line ( ax, ay ) ( bx, by ) =
    Canvas.shapes
        [ Canvas.Settings.stroke (Color.rgba 0 1 0 0.7)
        , Canvas.Settings.Line.lineWidth 5
        ]
        [ Canvas.path ( ax, ay )
            [ Canvas.lineTo ( bx, by )
            ]
        ]


type SocketType
    = Input
    | Output


type alias SocketRef =
    { id : Int
    , index : Int
    , socketType : SocketType
    }


type alias Connection =
    { input : SocketRef
    , output : SocketRef
    }
