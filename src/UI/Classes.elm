module UI.Classes exposing (..)

import Element
import Html.Attributes


spinner : Element.Attribute msg
spinner =
    Element.htmlAttribute <|
        Html.Attributes.class "spinner"


{-| animateTextColor 300
-}
animateTextColor : Int -> List (Element.Attribute msg)
animateTextColor timeInMillis =
    let
        transitionTime =
            String.fromInt timeInMillis ++ "ms ease"
    in
    [ Element.htmlAttribute <|
        Html.Attributes.style "transition" ("color " ++ transitionTime)
    , Element.htmlAttribute <|
        Html.Attributes.style "-webkit-transition" ("color " ++ transitionTime)
    ]
