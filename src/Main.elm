module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Connection exposing (Connection, Socket(..), getId, removePreviousConnection)
import Element exposing (Element, alignRight, el, fill, height, htmlAttribute, inFront, px, rgba, row, width)
import Element.Background as Background
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Elements exposing (canvasEl, codeFont, menuEl, nodeEl, shaderEl, white)
import Html exposing (Html)
import Html.Events.Extra.Mouse as Mouse
import Json.Encode as Encode
import List exposing (any, filter, head, map, maximum)
import Model exposing (Model, Msg(..), connecting, decodeStoredModel, removeSelected)
import Node exposing (Node, encode, setCode)
import Ports exposing (storeModel)
import Task
import Vec2 exposing (Vec2, encode)


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
    let
        center =
            Vec2.half model.windowSize
    in
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
                    ++ map inFront (map (nodeEl center) model.nodes)
                    ++ [ inFront (canvasEl model) ]
                )
                menuEl
            , Element.none
            ]
