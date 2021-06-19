module UI.Css exposing (..)

import Element exposing (..)
import Html.Attributes


spinner : Element.Attribute msg
spinner =
    htmlAttribute (Html.Attributes.class "spinner")


slowSpinner : Element.Attribute msg
slowSpinner =
    htmlAttribute (Html.Attributes.class "slowSpinner")


animationDuration : Float -> Element.Attribute msg
animationDuration duration =
    htmlAttribute (Html.Attributes.style "animation-duration" (String.fromFloat duration ++ "s"))


ignoreMouse : Bool -> Element.Attribute msg
ignoreMouse ignore =
    htmlAttribute
        (Html.Attributes.style "pointer-events"
            (if ignore then
                "none"

             else
                "auto"
            )
        )


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
    | BorderWidth
    | BorderColor
    | Translation
    | Scale


animatableToString animatable =
    case animatable of
        Height ->
            "height"

        Width ->
            "width"

        Shadow ->
            "box-shadow"

        BorderWidth ->
            "border-width"

        BorderColor ->
            "border-color"

        Translation ->
            "transform"

        Scale ->
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
