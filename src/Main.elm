module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Connection exposing (Socket(..), removePreviousConnection)
import Element exposing (el, fill, height, htmlAttribute, inFront, row, width)
import Element.Events as Events
import Elements exposing (canvasEl, codeEl, menuEl, nodeEl, shaderEl)
import Html exposing (Html)
import Html.Events.Extra.Mouse as Mouse
import Json.Encode as Encode
import List exposing (map)
import Model exposing (Model, Msg(..), connectSockets, connecting, decodeStoredModel, removeSelected, saveModel)
import Node exposing (deselect, move, nextId, select, setCode)
import Task
import Vec2 exposing (Vec2)


main : Program (Maybe Encode.Value) Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


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
                    | nodes = map (move delta) model.nodes
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
                    nextId model.nodes
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


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta UpdateTime
        , Browser.Events.onResize (\w h -> ResizeWindow (Vec2 (toFloat w) (toFloat h)))
        ]


clientPos : Mouse.Event -> Vec2
clientPos event =
    let
        ( x, y ) =
            event.clientPos
    in
    Vec2 x y
