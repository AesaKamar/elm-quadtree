module Main exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (style)
import Svg exposing (rect, svg, g)
import Svg.Attributes
import Window exposing (Size, resizes)
import QuadTree exposing (..)


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


type alias Model =
    { nodes : List NodeId
    , edges : List ( NodeId, NodeId )
    , quadTree : QuadTree (Bounded Int)
    , screenDims : Size
    }


init : ( Model, Cmd Msg )
init =
    ( { nodes = []
      , edges = []
      , quadTree = emptyQuadTree (boundingBox 0.0 0.0 0.0 0.0) 1000
      , screenDims = { height = 0, width = 0 }
      }
    , Cmd.none
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
        [ Html.div [] [ Html.text "Hello" ] ]
