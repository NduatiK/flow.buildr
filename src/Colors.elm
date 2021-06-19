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


midGrey : Color
midGrey =
    (rgb255 203 209 218)

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


orange : Color
orange =
    rgb255 232 125 125


darkGreen : Color
darkGreen =
    rgb255 0 150 136


withAlpha : Float -> Color -> Color
withAlpha alpha color =
    let
        rgbColor =
            toRgb color
    in
    rgba rgbColor.red rgbColor.green rgbColor.blue alpha


toString : Color -> String
toString color =
    let
        o =
            toRgb color
    in
    "rgba("
        ++ String.fromInt (round (o.red * 255))
        ++ ("," ++ String.fromInt (round (o.green * 255)))
        ++ ("," ++ String.fromInt (round (o.blue * 255)))
        ++ ("," ++ String.fromFloat o.alpha)
        ++ ")"


linearTransition : Float -> Color -> Color -> Color
linearTransition k start end =
    let
        ( s, e ) =
            ( toRgb start, toRgb end )

        tr s_ e_ =
            s_ + k * (e_ - s_)
    in
    rgba
        (tr s.red e.red)
        (tr s.green e.green)
        (tr s.blue e.blue)
        (tr s.alpha e.alpha)



--
