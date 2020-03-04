module Main exposing (main)

import Browser
import Browser.Events
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
import List exposing (any, filter, head, map, repeat)
import Node exposing (Node, decoder, encode, inputCount, outputCount, previewCode, setCode)
import Ports exposing (storeNodes)
import Shader exposing (fragmentShader, mesh, vertexShader)
import Vec2 exposing (Vec2, encode)
import WebGL


type alias Model =
    { nodes : List Node
    , dragging : Bool
    , lastCursorPos : Vec2
    , time : Float
    , connecting : Bool
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
      }
    , Cmd.none
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
    Browser.Events.onAnimationFrameDelta UpdateTime


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
    | StartConnect


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
            ( { model | nodes = Node.init :: map deselect model.nodes }
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

        StartConnect ->
            ( { model | connecting = True }
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
                 , Background.color darkGray
                 , Events.onDoubleClick Deselect
                 , Element.behindContent (shaderEl model.time)
                 , inFront (codeInput model.nodes)
                 ]
                    ++ map inFront (map nodeEl model.nodes)
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
            [ putsEl (outputCount node) alignTop
            , codePreviewEl node
            , putsEl (inputCount node) alignBottom
            ]
        )


-- rita upp linje med canvas overlay
-- från node.pos till node.pos med offset beroende på typ/antal


putsEl : Int -> Attribute Msg -> Element Msg
putsEl n alignment =
    row [ alignment, spacing 10, centerX ]
        (repeat n putEl)


putEl : Element Msg
putEl =
    el
        [ width (px 20)
        , height (px 20)
        , Background.color (rgb 0.9 0.3 0.3)
        , Events.onMouseDown StartConnect
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
