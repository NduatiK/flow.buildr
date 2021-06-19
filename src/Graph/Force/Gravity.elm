module Graph.Force.Gravity exposing (Vertex, run)

import Point2d
import Quantity
import Vector2d


type alias Point2d =
    Point2d.Point2d Float Float


type alias Vector2d =
    Vector2d.Vector2d Float Float


type alias Vertex =
    { id : Int
    , position : Point2d
    , velocity : Vector2d
    , gravityCenter : Point2d
    , gravityStrengthX : Float
    , gravityStrengthY : Float
    }


run : Float -> List Vertex -> List ( Int, Vector2d )
run alpha =
    let
        handle : Vertex -> ( Int, Vector2d )
        handle { id, position, velocity, gravityCenter, gravityStrengthX, gravityStrengthY } =
            let
                v : Vector2d
                v =
                    Vector2d.from position gravityCenter

                velocityDelta : Vector2d
                velocityDelta =
                    Vector2d.scaleBy (gravityStrengthX * alpha) v
            in
            ( id
            , Vector2d.sum [ velocity, velocityDelta ]
            )
    in
    List.map handle
