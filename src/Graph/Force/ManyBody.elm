module Graph.Force.ManyBody exposing (Vertex, run)

-- THIS FILE IS ORIGINALLY COPIED FROM THE SOURCE OF THE PACKAGE gampleman/elm-visualization

import BoundingBox2d
import Graph.Force.QuadTree as QuadTree
import Point2d
import Quantity
import Vector2d


type alias BoundingBox2d =
    BoundingBox2d.BoundingBox2d Float Float


type alias Vector2d =
    Vector2d.Vector2d Float Float


type alias Point2d =
    Point2d.Point2d Float Float


type alias Vertex comparable =
    { key : comparable
    , position : Point2d
    , velocity : Vector2d
    , strength : Float
    }


run : Float -> Float -> List (Vertex comparable) -> List (Vertex comparable)
run alpha theta vertices =
    let
        withAggregates =
            QuadTree.fromList .position vertices
                |> QuadTree.performAggregate config

        updateVertex vertex =
            { vertex | velocity = Vector2d.sum [ vertex.velocity, applyForce alpha theta withAggregates vertex ] }
    in
    List.map updateVertex vertices


type alias AggregateVertex =
    { position : Point2d
    , strength : Float
    }


{-| Combine a non-empty list of points into one superpoint with the average position and accumulated strength
-}
constructSuperPoint :
    { a | position : Point2d, strength : Float }
    -> List { a | position : Point2d, strength : Float }
    -> AggregateVertex
constructSuperPoint first rest =
    let
        initialPoint =
            Point2d.coordinates first.position

        initialStrength =
            first.strength

        folder point ( ( accumX, accumY ), strength, size ) =
            let
                ( x, y ) =
                    Point2d.coordinates point.position
            in
            ( ( accumX + Quantity.unwrap x, accumY + Quantity.unwrap y ), strength + point.strength, size + 1 )

        ( ( totalX, totalY ), totalStrength, totalSize ) =
            List.foldl folder ( Tuple.mapBoth Quantity.unwrap Quantity.unwrap initialPoint, initialStrength, 1 ) rest
    in
    { position =
        Point2d.xy (Quantity.Quantity (totalX / totalSize)) (Quantity.Quantity (totalY / totalSize))
    , strength = totalStrength
    }


config : QuadTree.Config AggregateVertex (Vertex a)
config =
    { toPoint = .position
    , combineVertices = constructSuperPoint
    , combineAggregates = constructSuperPoint
    }


applyForce : Float -> Float -> QuadTree.QuadTree AggregateVertex (Vertex comparable) -> Vertex comparable -> Vector2d
applyForce alpha theta qtree vertex =
    let
        -- based on https://en.wikipedia.org/wiki/Barnes%E2%80%93Hut_simulation#Calculating_the_force_acting_on_a_body
        -- when a group of other vertices is sufficiently far away, treat them as one vertex.
        -- its position is the average position, its strength the combined strength
        isFarAway : { a | boundingBox : BoundingBox2d, aggregate : AggregateVertex } -> Bool
        isFarAway treePart =
            let
                ( width, _ ) =
                    BoundingBox2d.dimensions treePart.boundingBox

                distance =
                    Point2d.distanceFrom vertex.position treePart.aggregate.position
            in
            Quantity.unwrap width / Quantity.unwrap distance < theta

        useAggregate : { a | boundingBox : BoundingBox2d, aggregate : AggregateVertex } -> Vector2d
        useAggregate treePart =
            calculateVelocity vertex treePart.aggregate

        calculateVelocity : { a | position : Point2d } -> { b | position : Point2d, strength : Float } -> Vector2d
        calculateVelocity target source =
            let
                delta : Vector2d
                delta =
                    Vector2d.from target.position source.position

                weight =
                    source.strength * alpha / Quantity.unwrap (Quantity.squared (Vector2d.length delta))
            in
            -- in rare cases, the delta can be the zero vector, and weight becomes NaN
            if isNaN weight then
                Vector2d.zero

            else
                Vector2d.scaleBy weight delta
    in
    case qtree of
        QuadTree.Empty ->
            Vector2d.zero

        QuadTree.Leaf leaf ->
            if isFarAway leaf then
                useAggregate leaf

            else
                let
                    ( first, rest ) =
                        leaf.children

                    applyForceFromPoint point accum =
                        -- don't distribute force to yourself
                        if point.key == vertex.key then
                            accum

                        else
                            Vector2d.sum [ calculateVelocity vertex point, accum ]
                in
                List.foldl applyForceFromPoint Vector2d.zero (first :: rest)

        QuadTree.Node node ->
            if isFarAway node then
                useAggregate node

            else
                let
                    helper tree =
                        applyForce alpha theta tree vertex
                in
                Vector2d.sum
                    [ helper node.nw
                    , helper node.ne
                    , helper node.se
                    , helper node.sw
                    ]
