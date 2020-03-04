module Main exposing (main)

import Browser
import Browser.Events
import Element exposing (Attribute, Color, Element, alignBottom, alignTop, centerX, centerY, column, el, fill, height, htmlAttribute, inFront, moveDown, moveRight, padding, px, rgb, row, spacing, text, width)
import Element.Background as Background
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes
import Html.Events.Extra.Mouse as Mouse
import Json.Decode as Decode
import Json.Encode as Encode
import List exposing (any, filter, head, map, repeat)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Node exposing (Node, decoder, encode, inputCount, outputCount, setCode)
import Ports exposing (storeNodes)
import Vec2 exposing (Vec2, encode)
import WebGL


type alias Model =
    { nodes : List Node
    , dragging : Bool
    , lastCursorPos : Vec2
    , time : Float
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
    Browser.Events.onAnimationFrameDelta TimeDelta


type Msg
    = Select Node
    | Deselect
    | StopDrag
    | Drag Vec2
    | Add
    | Remove
    | Save
    | SetCode String
    | TimeDelta Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Select node ->
            ( { model | nodes = map (select node) model.nodes, dragging = True }
            , Cmd.none
            )

        Deselect ->
            ( { model | nodes = map deselect model.nodes }
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

        SetCode code ->
            ( { model | nodes = map (setCode code) model.nodes }
            , Cmd.none
            )

        TimeDelta delta ->
            ( { model | time = model.time + delta }
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


red : Color
red =
    rgb 0.6 0.1 0.1


gray : Color
gray =
    rgb 0.3 0.3 0.3


darkGray : Color
darkGray =
    rgb 0.15 0.15 0.15


black : Color
black =
    rgb 0 0 0


white : Color
white =
    rgb 1 1 1


nodeColor : Node -> Color
nodeColor node =
    if node.selected then
        red

    else
        gray


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


codeInput : List Node -> Element Msg
codeInput nodes =
    if any (\n -> n.selected) nodes then
        Input.multiline
            [ height fill
            , width (px 300)
            , Background.color gray
            , Font.color white
            ]
            { label = Input.labelHidden "code"
            , onChange = SetCode
            , placeholder = Nothing
            , text = getSelectedCode nodes
            , spellcheck = False
            }

    else
        Element.none


perspective : Float -> Mat4
perspective t =
    Mat4.mul
        (Mat4.makePerspective 45 1 0.01 100)
        (Mat4.makeLookAt (vec3 (4 * cos t) 0 (4 * sin t)) (vec3 0 0 0) (vec3 0 1 0))


type alias Vertex =
    { position : Vec3
    , color : Vec3
    }


mesh : WebGL.Mesh Vertex
mesh =
    WebGL.triangles
        [ ( Vertex (vec3 1 1 1) (vec3 1 0 0)
          , Vertex (vec3 1 1 -1) (vec3 0 1 0)
          , Vertex (vec3 1 -1 -1) (vec3 0 0 1)
          )
        , ( Vertex (vec3 1 -1 -1) (vec3 0 0 1)
          , Vertex (vec3 1 -1 1) (vec3 0 0 0)
          , Vertex (vec3 1 1 1) (vec3 1 0 0)
          )
        ]


type alias Uniforms =
    { perspective : Mat4
    , time : Float
    }


vertexShader : WebGL.Shader Vertex Uniforms { vcolor : Vec3 }
vertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec3 color;
        uniform mat4 perspective;
        uniform float time;
        varying vec3 vcolor;

        void main () {
            gl_Position = perspective * vec4(position, 1.0);
            // gl_Position.x = 2.0;
            // gl_Position.y = sin(time) * 1000.0;
            // vcolor = color + sin(time);
            // gl_Position = vec4( position, 1.0 );
        }
    |]


fragmentShader : WebGL.Shader {} Uniforms { vcolor : Vec3 }
fragmentShader =
    [glsl|
        precision mediump float;
        varying vec3 vcolor;

        void main () {
            gl_FragColor = vec4(vec3(1.0, 0.0, 0.0), 1.0);
        }
    |]


glView : Float -> Element Msg
glView time =
    Element.html
        (WebGL.toHtml
            [ Html.Attributes.width 400
            , Html.Attributes.height 800
            ]
            [ WebGL.entity vertexShader fragmentShader mesh { perspective = perspective 1000, time = time / 1000 }
            ]
        )


view : Model -> Html Msg
view model =
    Element.layout [] <|
        row [ height fill, width fill ]
            [ el
                ([ width fill
                 , height fill
                 , htmlAttribute (Mouse.onMove (clientPos >> Drag))
                 , Events.onMouseUp StopDrag
                 , Background.color darkGray
                 , Events.onDoubleClick Deselect
                 , Element.behindContent (glView model.time)
                 ]
                    ++ map inFront (map nodeEl model.nodes)
                )
                menuEl
            , codeInput model.nodes
            ]


menuEl : Element Msg
menuEl =
    row
        [ alignBottom
        , spacing 5
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
        ]
        (text node.code)


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
            , Font.size 10
            ]
            [ putsEl (outputCount node) alignTop
            , codePreviewEl node
            , putsEl (inputCount node) alignBottom
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
