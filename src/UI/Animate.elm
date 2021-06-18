module UI.Animate exposing (opacity)

import Element exposing (..)
import Html.Attributes


class : String -> Attribute msg
class =
    Html.Attributes.class >> htmlAttribute


opacity : Attribute msg
opacity =
    class "animatesOpacityFast"
