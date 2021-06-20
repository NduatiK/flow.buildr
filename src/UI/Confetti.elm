module UI.Confetti exposing (..)

import Browser exposing (Document)
import Color
import Colors
import Element
import Html exposing (Html)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import MaterialIcons
import Particle exposing (Particle)
import Particle.System as System exposing (System)
import Random exposing (Generator)
import Random.Extra
import Random.Float exposing (normal)
import Svg exposing (..)
import Svg.Attributes as SAttrs exposing (..)


type Firework
    = Fizzler Element.Color


fizzler : Element.Color -> Generator (Particle Firework)
fizzler color =
    Particle.init (Random.constant (Fizzler color))
        |> Particle.withDirection (Random.map degrees (Random.float 0 360))
        |> Particle.withSpeed (Random.map (clamp 0 200) (normal 100 100))
        |> Particle.withLifetime (normal 0.85 0.75)


fireworkAt : Element.Color -> Float -> Float -> Generator (List (Particle Firework))
fireworkAt color x y =
    fizzler color
        |> Particle.withLocation (Random.constant { x = x, y = y })
        |> Particle.withGravity 0
        |> Particle.withDrag
            (\_ ->
                { coefficient = 1
                , density = 0.015
                , area = 4
                }
            )
        |> Random.list 30


view : System Firework -> Html msg
view model =
    System.view fireworkView
        [ Html.Attributes.style "width" "80px"
        , Html.Attributes.style "height" "80px"
        , Html.Attributes.style "overflow" "visible"
        ]
        model


fireworkView : Particle Firework -> Svg msg
fireworkView particle =
    case Particle.data particle of
        Fizzler color ->
            let
                length =
                    Basics.max 2 (Particle.speed particle / 15)

                ( hue, saturation, luminance ) =
                    toHsl color

                -- maxLuminance =
                --     100
                -- luminanceDelta =
                --     10
                lifetime =
                    Particle.lifetimePercent particle

                opacity =
                    if lifetime < 0.1 then
                        lifetime * 10

                    else
                        1
            in
            Svg.ellipse
                [ -- location within the burst
                  SAttrs.cx (String.fromFloat (length / 2))
                , SAttrs.cy "0"

                -- size, smeared by motion
                , SAttrs.rx (String.fromFloat length)
                , SAttrs.ry "2"
                , SAttrs.transform ("rotate(" ++ String.fromFloat (Particle.directionDegrees particle) ++ ")")

                -- color!
                , SAttrs.opacity (String.fromFloat opacity)
                , SAttrs.fill
                    --                 ((hslString
                    --     hue
                    --     saturation
                    --     luminance
                    --   -- (luminance - luminanceDelta * (1 - lifetime))
                    --  )
                    (toStr color)
                ]
                []



-- {-| Using the tango palette, but a little lighter. Original colors at
-- -}
-- toHsl : Color -> ( Float, Float, Float )
-- toHsl color =
--     case color of
--         Red ->
--             -- scarlet red
--             ( 0, 86, 75 )
--         Green ->
--             -- chameleon
--             ( 90, 75, 75 )
--         Blue ->
--             -- sky blue
--             ( 211, 49, 83 )


{-| Using the tango palette, but a little lighter. Original colors at
-}
toHsl : Element.Color -> ( Float, Float, Float )
toHsl color =
    color
        |> MaterialIcons.colorToMaterialColor
        |> Color.toHsla
        |> (\x ->
                ( x.hue, x.saturation, x.lightness )
           )
        |> (\( h, s, l ) -> ( 255 * h, 100 * s, 100 * l ))


toStr : Element.Color -> String
toStr color =
    color
        |> MaterialIcons.colorToMaterialColor
        |> Color.toCssString


hslString : Float -> Float -> Float -> String
hslString hue saturation luminance =
    "hsl("
        ++ String.fromFloat hue
        ++ ","
        ++ String.fromFloat saturation
        ++ "%,"
        ++ String.fromFloat luminance
        ++ "%)"
