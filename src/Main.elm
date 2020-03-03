module Main exposing (main)

import Browser
import Element exposing (Attribute, Color, Element, alignBottom, alignTop, centerX, column, el, fill, height, htmlAttribute, inFront, moveDown, moveRight, padding, px, rgb, row, spacing, text, width)
import Element.Background as Background
import Element.Events as Events
import Element.Input as Input
import Html exposing (Html)
import Html.Events.Extra.Mouse as Mouse
import List exposing (filter, map, repeat)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


type alias Vec2 =
    { x : Float
    , y : Float
    }


add : Vec2 -> Vec2 -> Vec2
add a b =
    Vec2 (a.x + b.x) (a.y + b.y)


sub : Vec2 -> Vec2 -> Vec2
sub a b =
    Vec2 (a.x - b.x) (a.y - b.y)


type alias Node =
    { pos : Vec2
    , selected : Bool
    , code : String
    }


type alias Model =
    { nodes : List Node
    , dragging : Bool
    , lastCursorPos : Vec2
    }


init : Model
init =
    { nodes =
        [ { pos = Vec2 100 100, selected = False, code = "a" }
        , { pos = Vec2 200 200, selected = False, code = "b" }
        ]
    , dragging = False
    , lastCursorPos = Vec2 0 0
    }


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
        { node | pos = add node.pos offset }

    else
        node


type Msg
    = Select Node
    | StopDrag
    | Drag Vec2
    | Add
    | Remove


update : Msg -> Model -> Model
update msg model =
    case msg of
        Select node ->
            { model | nodes = map (select node) model.nodes, dragging = True }

        StopDrag ->
            { model | dragging = False }

        Drag pos ->
            if model.dragging then
                { model | nodes = map (drag (sub pos model.lastCursorPos)) model.nodes, lastCursorPos = pos }

            else
                { model | lastCursorPos = pos }

        Add ->
            { model | nodes = new :: map deselect model.nodes }

        Remove ->
            { model | nodes = filter (\n -> not n.selected) model.nodes }


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


viewNode : Node -> Element Msg
viewNode node =
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
            [ puts 3 alignTop
            , puts 3 alignBottom
            ]
        )


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


menu : Element Msg
menu =
    row
        [ alignBottom
        , centerX
        , spacing 5
        ]
        [ addButton, removeButton ]


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
                    ++ map inFront (map viewNode model.nodes)
                )
                menu
            , el
                [ width (px 300)
                , height fill
                , Background.color gray
                ]
                Element.none
            ]


puts : Int -> Attribute msg -> Element msg
puts n alignment =
    row [ alignment, spacing 10, centerX ]
        (repeat n put)


put : Element msg
put =
    el
        [ width (px 20)
        , height (px 20)
        , Background.color (rgb 0.9 0.3 0.3)
        ]
        Element.none
