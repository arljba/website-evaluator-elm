module ForceDirectedGraph exposing (..)

import Color exposing (Color)
import Force exposing (State)
import Graph exposing (Edge, Graph, Node, NodeContext, NodeId)
import Html exposing (Html)
import TypedSvg exposing (..)
import TypedSvg.Attributes exposing (class, fill, stroke, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, r, strokeWidth, x1, x2, y1, y2)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (..)


width : Float
width =
    990


height : Float
height =
    700


color : Color
color =
    Color.rgba 0.39 0.65 0.86 1


type alias Entity =
    Force.Entity NodeId { value : String }


initializeNode : NodeContext String () -> NodeContext Entity ()
initializeNode ctx =
    { node = { label = Force.entity ctx.node.id ctx.node.label, id = ctx.node.id }
    , incoming = ctx.incoming
    , outgoing = ctx.outgoing
    }


initGraph : Graph String () -> Graph Entity ()
initGraph myGraph =
    let
        graph =
            Graph.mapContexts initializeNode
                myGraph

        links =
            graph
                |> Graph.edges
                |> List.map
                    (\{ from, to } ->
                        { source = from
                        , target = to
                        , distance = 70
                        , strength = Nothing
                        }
                    )

        forces =
            [ Force.customLinks 1 links
            , Force.manyBodyStrength -30 <| List.map .id <| Graph.nodes graph
            , Force.center (width / 2) (height / 2)
            ]
    in
    Graph.nodes graph
        |> List.map .label
        |> Force.computeSimulation (Force.simulation forces)
        |> updateGraphWithList graph


updateGraphWithList : Graph Entity () -> List Entity -> Graph Entity ()
updateGraphWithList =
    let
        graphUpdater value =
            Maybe.map (\ctx -> updateContextWithValue ctx value)
    in
    List.foldr (\node graph -> Graph.update node.id (graphUpdater node) graph)


updateContextWithValue nodeCtx value =
    let
        node =
            nodeCtx.node
    in
    { nodeCtx | node = { node | label = value } }


linkElement : Graph Entity () -> Edge () -> Svg msg
linkElement graph edge =
    let
        init =
            Maybe.withDefault (Force.entity 0 "") << Maybe.map (.node >> .label)

        source =
            init <| Graph.get edge.from graph

        target =
            init <| Graph.get edge.to graph
    in
    line
        [ strokeWidth 1.5
        , stroke <| Paint <| color
        , x1 source.x
        , y1 source.y
        , x2 target.x
        , y2 target.y
        ]
        []


nodeElement node =
    circle
        [ r 6
        , fill <| Paint Color.black
        , stroke <| Paint <| Color.rgba 0 0 0 0
        , strokeWidth 7
        , cx node.label.x
        , cy node.label.y
        ]
        [ title [] [ text node.label.value ] ]


viewGraph forceGraph =
    svg [ viewBox 0 0 width height ]
        [ Graph.edges forceGraph
            |> List.map (linkElement forceGraph)
            |> g [ class [ "links" ] ]
        , Graph.nodes forceGraph
            |> List.map nodeElement
            |> g [ class [ "nodes" ] ]
        ]
