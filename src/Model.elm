module Model exposing (Model, Msg(..), SavedModel, connectSockets, connecting, decodeStoredModel, removeSelected, saveModel)

import Browser.Dom
import Connection exposing (Connection, Socket(..), connectionHasNoNode, getId)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import List exposing (any, filter)
import Node exposing (Node)
import Ports exposing (storeModel)
import Vec2 exposing (Vec2)


type alias Model =
    { nodes : List Node
    , connections : List Connection
    , dragging : Bool
    , lastCursorPos : Vec2
    , time : Float
    , windowSize : Vec2
    , connectingSocket : Maybe Socket
    }


type alias SavedModel =
    { nodes : List Node
    , connections : List Connection
    }


connecting : Model -> Bool
connecting model =
    case model.connectingSocket of
        Just _ ->
            True

        Nothing ->
            False


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
    | StartConnect Socket
    | ResizeWindow Vec2
    | InitWindowSize Browser.Dom.Viewport
    | Connect Socket


encodeModel : Model -> Encode.Value
encodeModel model =
    Encode.object
        [ ( "nodes", Encode.list Node.encode model.nodes )
        , ( "connections", Encode.list Connection.encode model.connections )
        ]


modelDecoder : Decoder SavedModel
modelDecoder =
    Decode.succeed SavedModel
        |> required "nodes" (Decode.list Node.decoder)
        |> required "connections" (Decode.list Connection.decoder)


decodeStoredModel : Encode.Value -> SavedModel
decodeStoredModel modelJson =
    case Decode.decodeValue modelDecoder modelJson of
        Ok savedModel ->
            savedModel

        Err _ ->
            { nodes = [], connections = [] }


removeSelected : Model -> Model
removeSelected model =
    let
        nodesToRemove =
            filter (\n -> n.selected) model.nodes

        nodes =
            filter (\n -> not n.selected) model.nodes

        connections =
            filter (connectionHasNoNode nodesToRemove) model.connections
    in
    { model | nodes = nodes, connections = connections }


saveModel : Model -> Cmd msg
saveModel model =
    encodeModel model
        |> Ports.storeModel


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

        selfReferencing =
            getId input == getId output

        valid =
            inputExists
                && inputIsInput
                && outputExists
                && outputIsOutput
                && not wouldBeDuplicate
                && not selfReferencing
    in
    if valid then
        Just { input = input, output = output }

    else
        Nothing
