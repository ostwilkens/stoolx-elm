module Model exposing (Model, Msg(..), SavedModel, connecting, decodeStoredModel, removeSelected)

import Browser.Dom
import Connection exposing (Connection, Socket, connectionHasNoNode)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import List exposing (filter)
import Node exposing (Node)
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
