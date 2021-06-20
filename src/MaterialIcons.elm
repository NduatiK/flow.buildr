module MaterialIcons exposing (colorToMaterialColor, material)

import Color as MaterialColor
import Element exposing (..)
import Element.Background
import Material.Icons
import Material.Icons.Outlined
import Material.Icons.Types exposing (Coloring(..), Icon)


{-|

    MaterialIcons.material []
        { icon = Material.Icons.error
        , size = 16
        , color = Colors.white
        }

-}
material : List (Attribute msg) -> { icon : Icon msg, size : Int, color : Color } -> Element msg
material attr { icon, size, color } =
    let
        materialColor =
            Color <| colorToMaterialColor color
    in
    icon size materialColor
        |> html
        |> el ([] ++ attr)


colorToMaterialColor : Element.Color -> MaterialColor.Color
colorToMaterialColor color =
    let
        colorParts =
            toRgb color
    in
    MaterialColor.rgba colorParts.red colorParts.green colorParts.blue colorParts.alpha
