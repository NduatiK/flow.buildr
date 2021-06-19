module Graph.Force.Link exposing (Param, run)

import IntDict exposing (IntDict)
import Pixels
import Point2d
import Quantity
import Vector2d


type alias Vector2d =
    Vector2d.Vector2d Float Float


type alias Point2d =
    Point2d.Point2d Float Float


type alias Param =
    { source : { id : Int, degree : Int, position : Point2d, velocity : Vector2d }
    , target : { id : Int, degree : Int, position : Point2d, velocity : Vector2d }
    , distance : Float
    , strength : Float
    }


run : Float -> List Param -> List ( Int, Vector2d )
run alpha =
    let
        handle : Param -> IntDict Vector2d -> IntDict Vector2d
        handle { source, target, distance, strength } =
            let
                diff =
                    Vector2d.from
                        (source.position
                            |> Point2d.translateBy source.velocity
                        )
                        (target.position
                            |> Point2d.translateBy target.velocity
                        )

                d =
                    Vector2d.length diff

                l =
                    alpha * strength * (Quantity.unwrap d - distance) / Quantity.unwrap d

                f =
                    diff |> Vector2d.scaleBy l

                bias =
                    toFloat source.degree / toFloat (source.degree + target.degree)

                updateSourceVelocity mV =
                    Just
                        (Vector2d.sum
                            [ mV |> Maybe.withDefault source.velocity
                            , Vector2d.scaleBy (1 - bias) f
                            ]
                        )

                updateTargetVelocity mV =
                    Just
                        (Vector2d.minus
                            (mV |> Maybe.withDefault target.velocity)
                            (Vector2d.scaleBy bias f)
                        )
            in
            IntDict.update source.id updateSourceVelocity >> IntDict.update target.id updateTargetVelocity
    in
    List.foldr handle IntDict.empty >> IntDict.toList
