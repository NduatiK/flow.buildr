module Colors exposing (..)

import Element exposing (..)


black : Color
black =
    rgb255 0 0 0


white : Color
white =
    rgb 1 1 1


grey : Color
grey =
    rgb255 143 139 168


lightGrey : Color
lightGrey =
    rgb255 244 243 246


purpleHex : String
purpleHex =
    "#6637f1"


purple : Color
purple =
    rgb255 102 55 241


darkBlue : Color
darkBlue =
    rgb255 77 119 243


blue : Color
blue =
    rgb255 92 192 243


teal : Color
teal =
    rgb255 84 218 210


green : Color
green =
    rgb255 110 228 193


lime : Color
lime =
    rgb255 214 228 40


withAlpha : Float -> Color -> Color
withAlpha alpha color =
    let
        rgbColor =
            toRgb color
    in
    rgba rgbColor.red rgbColor.green rgbColor.blue alpha
