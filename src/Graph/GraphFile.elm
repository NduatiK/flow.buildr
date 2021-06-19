module Graph.GraphFile exposing
    ( FlowGraph
    , GraphFile
    , actionDefaultVertexProp
    , default
    , edgeTransition
    , forceTick
    , getVertices
    , transitionGraphFile
    , vertexTransition
    )

import Color
import Colors
import Ease
import Element exposing (Color)
import Graph exposing (Edge, Graph, Node, NodeId)
import Graph.Extra
import Graph.Force as Force exposing (ForceGraph)
import Material.Icons
import Material.Icons.Types
import Pixels
import Point2d
import Quantity
import Set
import Vector2d exposing (Vector2d)


type GraphFile
    = GraphFile
        { graph : FlowGraph

        -- , bags : BagDict
        , defaultVertexProperties : VertexProperties
        , defaultEdgeProperties : EdgeProperties
        }


{-| only needed by the decoder
-}
new =
    GraphFile



--------------
-- Defaults --
--------------


default : GraphFile
default =
    GraphFile
        { graph =
            Graph.fromNodesAndEdges
                [ Node 1 actionDefaultVertexProp
                , Node 2
                    { actionDefaultVertexProp
                        | color = Colors.white
                        , position = Point2d.xy (Quantity.Quantity -175) (Quantity.Quantity 148)
                    }
                , Node 3
                    { actionDefaultVertexProp
                        | color = Colors.white
                        , position = Point2d.xy (Quantity.Quantity 175) (Quantity.Quantity 148)
                    }

                --      { position = Point2d.fromPixels { x = 0, y = 0 }
                --   , velocity = Point2d.fromPixels { x = 0, y = 0 }
                --   , manyBodyStrength = 0
                --   , gravityCenter = Point2d.fromPixels { x = 0, y = 0 }
                --   , gravityStrengthX = 0
                --   , gravityStrengthY = 0
                --   , fixed = Bool
                --   , isExpanded = Bool
                --   }
                ]
                [ Edge 1 2 actionDefaultEdgeProp
                , Edge 1 3 actionDefaultEdgeProp
                ]

        -- , bags = Dict.empty
        , defaultVertexProperties = actionDefaultVertexProp
        , defaultEdgeProperties = actionDefaultEdgeProp
        }


actionDefaultVertexProp : VertexProperties
actionDefaultVertexProp =
    { position = Point2d.origin
    , velocity = Vector2d.zero
    , gravityCenter = Point2d.xy (Quantity.Quantity 0) (Quantity.Quantity 0)
    , gravityStrengthX = 0.05
    , gravityStrengthY = 0.05
    , manyBodyStrength = -100

    --   ,   label = ""
    -- , labelSize = 12
    -- , labelPosition = LabelTop
    -- , labelColor = Colors.white
    -- , labelIsVisible = True
    , color = Colors.orange
    , radius = 54 / 2
    , borderColor = Colors.lightGrey
    , borderWidth = 0

    -- , opacity = 1
    -- , inBags = Set.empty
    , fixed = False
    , isExpanded = True
    }


actionDefaultEdgeProp : EdgeProperties
actionDefaultEdgeProp =
    { label = ""
    , labelSize = 12
    , labelColor = Colors.lightGrey
    , labelIsVisible = True
    , color = Colors.lightGrey
    , thickness = 3
    , distance = 50
    , strength = 0.7
    , opacity = 1
    }


type alias FlowGraph =
    ForceGraph VertexProperties EdgeProperties



-- type alias FlowGraph msg =
--     { nodes : Node msg
--     , vertexes : Vertex
--     }


type alias VertexProperties =
    { position : Point2d
    , velocity : Velocity
    , manyBodyStrength : Float
    , gravityCenter : Point2d
    , gravityStrengthX : Float
    , gravityStrengthY : Float
    , radius : Float
    , color : Color
    , borderColor : Color
    , borderWidth : Float
    , fixed : Bool
    , isExpanded : Bool
    }


type alias EdgeProperties =
    { label : String
    , labelSize : Float
    , labelColor : Color
    , labelIsVisible : Bool
    , distance : Float
    , strength : Float
    , thickness : Float
    , color : Color
    , opacity : Float
    }



-- type Node msg
--     = Expanded (Config msg)
--     | Collapsed (Config msg)


type Vertex
    = Vertex


type alias ForceVertex n =
    { n
        | position : Point2d
        , velocity : Velocity
        , manyBodyStrength : Float
        , gravityCenter : Point2d
        , gravityStrengthX : Float
        , gravityStrengthY : Float
        , fixed : Bool
    }


type alias Velocity =
    Vector2d Float Float


type alias Point2d =
    Point2d.Point2d Float Float


type alias ForceEdge e =
    { e
        | distance : Float
        , strength : Float
    }


getVertices : GraphFile -> List (Node VertexProperties)
getVertices (GraphFile { graph }) =
    Graph.nodes graph


getEdges : GraphFile -> List (Edge EdgeProperties)
getEdges (GraphFile { graph }) =
    Graph.edges graph


getGraph : GraphFile -> FlowGraph
getGraph (GraphFile { graph }) =
    graph


mapGraph : (FlowGraph -> FlowGraph) -> GraphFile -> GraphFile
mapGraph f (GraphFile p) =
    GraphFile { p | graph = f p.graph }


setGraph : FlowGraph -> GraphFile -> GraphFile
setGraph g =
    mapGraph (always g)



----------------------------------
-- Animation Related Operations --
----------------------------------


forceTick : Force.State -> GraphFile -> ( Force.State, GraphFile )
forceTick forceState (GraphFile p) =
    let
        ( newForceState, newGraph ) =
            Force.tick forceState p.graph
    in
    ( newForceState, GraphFile { p | graph = newGraph } )


transitionGraphFile : Float -> { start : GraphFile, end : GraphFile } -> GraphFile
transitionGraphFile elapsedTimeRatio { start, end } =
    let
        eTR =
            -- elapsed time
            -- in order to prevent flickering at the very end of tranition we clamp it.
            clamp 0 1 elapsedTimeRatio

        { result, nodeSeparation, edgeSeparation } =
            Graph.Extra.union (getGraph start) (getGraph end)

        ( verticesInStartButNotInEnd, verticesInIntersection, verticesInEndButNotInStart ) =
            nodeSeparation

        upVerticesInStartButNotInEnd =
            Graph.Extra.updateNodes
                (verticesInStartButNotInEnd
                    |> List.map .id
                    |> Set.fromList
                )
                (\vP ->
                    { vP
                        | radius =
                            Ease.reverse Ease.inCubic eTR
                                * vP.radius

                        -- , labelSize =
                        --     Ease.reverse Ease.inCubic eTR
                        --         * vP.labelSize
                        , borderWidth =
                            Ease.reverse Ease.inCubic eTR
                                * vP.borderWidth
                    }
                )

        upVerticesInEndButNotInStart =
            Graph.Extra.updateNodes
                (verticesInEndButNotInStart
                    |> List.map .id
                    |> Set.fromList
                )
                (\vP ->
                    { vP
                        | radius =
                            Ease.inCubic eTR * vP.radius

                        -- , labelSize =
                        --     Ease.inCubic eTR * vP.labelSize
                        , borderWidth =
                            Ease.inCubic eTR * vP.borderWidth
                    }
                )

        upVerticesInIntersection =
            Graph.Extra.updateNodesBy
                (verticesInIntersection
                    |> List.map (\{ id, label } -> ( id, label ))
                )
                (\endVertex startVertex ->
                    vertexTransition eTR
                        startVertex
                        endVertex
                )

        ( edgesInStartButNotInEnd, edgesInIntersection, edgesInEndButNotInStart ) =
            edgeSeparation

        upEdgesInStartButNotInEnd =
            Graph.Extra.updateEdges
                (edgesInStartButNotInEnd
                    |> List.map (\{ from, to } -> ( from, to ))
                    |> Set.fromList
                )
                (\eP ->
                    { eP
                        | thickness =
                            (1 - Ease.outQuint eTR) * eP.thickness
                        , labelSize =
                            Ease.reverse Ease.inCubic eTR * eP.labelSize
                    }
                )

        upEdgesInEndButNotInStart =
            Graph.Extra.updateEdges
                (edgesInEndButNotInStart
                    |> List.map (\{ from, to } -> ( from, to ))
                    |> Set.fromList
                )
                (\eP ->
                    { eP
                        | thickness = Ease.inQuint eTR * eP.thickness
                        , labelSize = Ease.inCubic eTR * eP.labelSize
                    }
                )

        upEdgesInIntersection =
            Graph.Extra.updateEdgesBy
                (edgesInIntersection
                    |> List.map (\{ from, to, label } -> ( ( from, to ), label ))
                )
                (\endEdge startEdge ->
                    edgeTransition eTR
                        startEdge
                        endEdge
                )
    in
    end
        -- |> removeAllBags
        |> setGraph
            (result
                |> upVerticesInStartButNotInEnd
                |> upVerticesInEndButNotInStart
                |> upVerticesInIntersection
                |> upEdgesInStartButNotInEnd
                |> upEdgesInEndButNotInStart
                |> upEdgesInIntersection
            )


vertexTransition : Float -> VertexProperties -> VertexProperties -> VertexProperties
vertexTransition eTR startVertex endVertex =
    { startVertex
        | position =
            startVertex.position
                |> Point2d.translateBy
                    (Vector2d.scaleBy (Ease.inOutCubic eTR)
                        (Vector2d.from startVertex.position endVertex.position)
                    )
        , radius =
            startVertex.radius
                + (eTR * (endVertex.radius - startVertex.radius))

        -- , label =
        --     if eTR < 0.5 then
        --         startVertex.label
        --     else
        --         endVertex.label
        -- , labelSize =
        --     if startVertex.label == endVertex.label then
        --         startVertex.labelSize
        --             + (eTR * (endVertex.labelSize - startVertex.labelSize))
        --     else if eTR < 0.5 then
        --         startVertex.labelSize * Ease.reverse Ease.inCubic (2 * eTR)
        --     else
        --         endVertex.labelSize * Ease.inCubic (2 * (eTR - 0.5))
        -- , labelColor =
        --     Colors.linearTransition eTR
        --         startVertex.labelColor
        --         endVertex.labelColor
        , borderWidth =
            startVertex.borderWidth
                + (eTR * (endVertex.borderWidth - startVertex.borderWidth))
        , borderColor =
            Colors.linearTransition eTR
                startVertex.borderColor
                endVertex.borderColor

        -- , opacity =
        --     startVertex.opacity
        --         + (eTR * (endVertex.opacity - startVertex.opacity))
        , color =
            Colors.linearTransition eTR
                startVertex.color
                endVertex.color
    }


edgeTransition : Float -> EdgeProperties -> EdgeProperties -> EdgeProperties
edgeTransition eTR startEdge endEdge =
    { startEdge
        | thickness =
            startEdge.thickness
                + (eTR * (endEdge.thickness - startEdge.thickness))
        , label =
            if eTR < 0.5 then
                startEdge.label

            else
                endEdge.label
        , labelSize =
            if startEdge.label == endEdge.label then
                startEdge.labelSize
                    + (eTR * (endEdge.labelSize - startEdge.labelSize))

            else if eTR < 0.5 then
                startEdge.labelSize * Ease.reverse Ease.inCubic (2 * eTR)

            else
                endEdge.labelSize * Ease.inCubic (2 * (eTR - 0.5))
        , labelColor =
            Colors.linearTransition eTR
                startEdge.labelColor
                endEdge.labelColor
        , color =
            Colors.linearTransition eTR startEdge.color endEdge.color
        , opacity =
            startEdge.opacity
                + (eTR * (endEdge.opacity - startEdge.opacity))
    }
