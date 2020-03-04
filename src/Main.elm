module Main exposing (main)

import Browser
import Element exposing (Attribute, Color, Element, alignBottom, alignTop, centerX, column, el, fill, height, htmlAttribute, inFront, moveDown, moveRight, padding, px, rgb, row, spacing, text, width)
import Element.Background as Background
import Element.Events as Events
import Element.Input as Input
import Html exposing (Html)
import Html.Events.Extra.Mouse as Mouse
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (hardcoded, required)
import Json.Encode as Encode
import List exposing (filter, map, repeat)
import Node exposing (Node, decoder, encode)
import Ports exposing (storeNodes)
import Vec2 exposing (Vec2, encode)


main : Program (Maybe Encode.Value) Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type Msg
    = Select Node
    | StopDrag
    | Drag Vec2
    | Add
    | Remove
    | Save


type alias Model =
    { nodes : List Node
    , dragging : Bool
    , lastCursorPos : Vec2
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
      }
    , Cmd.none
    )


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


new : Node
new =
    { pos = Vec2 200 300, selected = True, code = "x" }


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Select node ->
            ( { model | nodes = map (select node) model.nodes, dragging = True }
            , Cmd.none
            )

        StopDrag ->
            ( { model | dragging = False }
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
            ( { model | nodes = new :: map deselect model.nodes }
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


red : Color
red =
    rgb 0.9 0.1 0.1


gray : Color
gray =
    rgb 0.3 0.3 0.3


black : Color
black =
    rgb 0 0 0


nodeColor : Node -> Color
nodeColor node =
    if node.selected then
        red

    else
        gray


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
        , padding 5
        ]
        { label = text "add"
        , onPress = Just Add
        }


removeButton : Element Msg
removeButton =
    Input.button
        [ Background.color red
        , padding 5
        ]
        { label = text "remove"
        , onPress = Just Remove
        }


saveButton : Element Msg
saveButton =
    Input.button
        [ Background.color red
        , padding 5
        ]
        { label = text "save"
        , onPress = Just Save
        }


view : Model -> Html Msg
view model =
    Element.layout [] <|
        row [ height fill, width fill ]
            [ el
                ([ width fill
                 , height fill
                 , htmlAttribute (Mouse.onMove (clientPos >> Drag))
                 , Events.onMouseUp StopDrag
                 , Background.color black
                 ]
                    ++ map inFront (map nodeEl model.nodes)
                )
                menuEl
            , el
                [ width (px 300)
                , height fill
                , Background.color gray
                ]
                Element.none
            ]


menuEl : Element Msg
menuEl =
    row
        [ alignBottom
        , centerX
        , spacing 5
        ]
        [ addButton
        , removeButton
        , saveButton
        ]


nodeEl : Node -> Element Msg
nodeEl node =
    el
        [ moveRight node.pos.x
        , moveDown node.pos.y
        ]
        (column
            [ Background.color (nodeColor node)
            , width (px 100)
            , height (px 100)
            , spacing 20
            , Events.onMouseDown (Select node)
            ]
            [ putsEl 3 alignTop
            , putsEl 3 alignBottom
            ]
        )


putsEl : Int -> Attribute msg -> Element msg
putsEl n alignment =
    row [ alignment, spacing 10, centerX ]
        (repeat n putEl)


putEl : Element msg
putEl =
    el
        [ width (px 20)
        , height (px 20)
        , Background.color (rgb 0.9 0.3 0.3)
        ]
        Element.none
