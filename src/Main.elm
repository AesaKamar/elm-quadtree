module Main exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (style)
import Svg exposing (rect, svg, g)
import Svg.Attributess
import Window exposing (Size, resizes)
import QuadTree exposing (..)
import Task


type Msg
    = WindowResize Window.Size


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias NodeId =
    Int


type alias BoundedPoint =
    { x : Float, y : Float, boundingBox : BoundingBox }


type alias Model =
    { nodes : List NodeId
    , edges : List ( NodeId, NodeId )
    , quadTree : QuadTree BoundedPoint
    , screenDims : Size
    }


init : ( Model, Cmd Msg )
init =
    ( { nodes = []
      , edges = []
      , quadTree = emptyQuadTree (boundingBox 0.0 0.0 0.0 0.0) 1000
      , screenDims = { height = 0, width = 0 }
      }
    , Task.perform WindowResize Window.size
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WindowResize windowSizing ->
            let
                horizontalInterval =
                    { low = 0.0, high = windowSizing.width |> toFloat }

                verticalInterval =
                    { low = 0.0, high = windowSizing.height |> toFloat }

                updatedQuadTree =
                    reset { horizontal = horizontalInterval, vertical = verticalInterval } model.quadTree
            in
                ( Model model.nodes model.edges updatedQuadTree windowSizing
                , Cmd.none
                )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Window.resizes WindowResize



-- VIEW


viewBoxSvgAttr : Window.Size -> Svg.Attribute Msg
viewBoxSvgAttr bb =
    let
        pts =
            [ 0
            , 0
            , bb.width
            , bb.height
            ]

        pointsAsString =
            String.join " " (List.map toString pts)
    in
        Svg.Attributes.viewBox pointsAsString


view : Model -> Html Msg
view model =
    svg
        [ Svg.Attributes.width ((model.screenDims.width |> toString) ++ "px")
        , Svg.Attributes.height ((model.screenDims.height |> toString) ++ "px")
        , viewBoxSvgAttr model.screenDims
        , Html.Attributes.style [ ( "outline-offset", "-10px" ) ]
        ]
        [ Svg.rect
            [ Svg.Attributes.width "300px"
            , Svg.Attributes.height "300px"
            ]
            []
        ]



-- TODO Do tail call elimination on this thing


viewQuadTree : QuadTree -> Svg.Svg
viewQuadTree quadTree =
    case quadTree of
        Leaf bb maxItems elems ->
            viewLeaf bb maxItems elems

        Node bb ne nw sw se ->
            viewNode bb ne nw sw se


viewLeaf : BoundingBox -> Int -> Array a -> Svg.Svg
viewLeaf bb maxItems elems =
    Svg.g
        [ Svg.Attribute.x (bb.horizontal.low <| toString ++ "px")
        , Svg.Attribute.y (bb.vertical.low <| toString ++ "px")
        , Svg.Attribute.width
            ((bb.horizontal.high - bb.horizontal.low) <| toString ++ "px")
        , Svg.Attribute.height ((bb.vertical.high - bb.vertical.low) <| toString ++ "px")
        ]
        []


viewNode : BoundingBox -> QuadTree a -> QuadTree a -> QuadTree a -> QuadTree a -> Svg.Svg
viewNode bb ne nw sw se =
    Svg.g [] []
