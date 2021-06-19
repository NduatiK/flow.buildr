module UI.Css exposing (..)

import Element exposing (..)
import Html.Attributes


translateXY : Float -> Float -> Element.Attribute msg
translateXY x y =
    htmlAttribute
        (Html.Attributes.style "transform"
            (String.join
                ""
                [ "translate3d(", String.fromFloat x, "px,", String.fromFloat y, "px,0px)" ]
            )
        )


type Animatable
    = Height
    | Width
    | Shadow
    | Translation


animatableToString animatable =
    case animatable of
        Height ->
            "height"

        Width ->
            "width"

        Shadow ->
            "box-shadow"

        Translation ->
            "transform"


transition : List ( Animatable, Float, String ) -> Element.Attribute msg
transition items =
    htmlAttribute
        (Html.Attributes.style "transition"
            (String.join ", " <|
                List.map
                    (\( attr, time, curve ) ->
                        String.join " " [ animatableToString attr, String.fromFloat time ++ "ms", curve ]
                    )
                    items
            )
        )
