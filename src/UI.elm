module UI exposing
    ( Palette
    , black
    , centerWrap
    , colorAnimatingIcon
    , customIcon
    , customIconWThickness
    , customLayout
    , darkGreen
    , darkness
    , defaultPalette
    , destructiveRed
    , getSizeOfWindow
    , grayText
    , green
    , gridIcon
    , layout
    , orange
    , raisedEl
    , red
    , showIf
    , sidebarWidth
    , textColor
    , transparent
    , white
    , withAlpha
    , wrapButtonChild
    )

import Browser.Dom
import Colors
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import FeatherIcons
import Gen.Route exposing (Route(..))
import Html
import Html.Attributes
import Shared
import Svg
import Svg.Attributes exposing (points, x1, x2, y1, y2)
import Task


sidebarPages : List SidebarElement
sidebarPages =
    [ Logo
    , Page ( "Home", FeatherIcons.target, A )
    , Page ( "Home", FeatherIcons.gitPullRequest, Flow_buildr )
    , Page ( "Home", FeatherIcons.settings, A )
    , Page ( "Home", FeatherIcons.pieChart, A )
    , Page ( "Home", FeatherIcons.code, A )
    ]


layout : Route -> Palette -> Element msg -> Element msg
layout currentRoute palette child =
    customLayout currentRoute palette [] child


sidebarWidth =
    78


customLayout : Route -> Palette -> List (Attribute msg) -> Element msg -> Element msg
customLayout currentRoute palette sidebarAttr child =
    row
        [ htmlAttribute (Html.Attributes.style "transition" "color 200ms, background-color 200ms")
        , htmlAttribute (Html.Attributes.style "height" "100vh")

        -- , scrollbarY
        , width fill

        -- , height fill
        , Font.color textColor
        ]
        [ sidebar sidebarAttr currentRoute palette
        , el [ htmlAttribute (Html.Attributes.style "height" "100vh"), width fill, scrollbarY ] child
        ]


sidebar : List (Attribute msg) -> Route -> Palette -> Element msg
sidebar sidebarAttr currentRoute palette =
    let
        pages =
            bdPages currentRoute
                sidebarPages
                palette

        bgColor =
            if palette.isDark then
                palette.darkBackgroundColor

            else
                rgb255 248 247 251

        bdColor =
            if palette.isDark then
                palette.backgroundColor

            else
                palette.darkBackgroundColor
    in
    pages
        |> column
            [ paddingEach { bottom = 50, left = 28 - 8, right = 24 - 8, top = 50 }
            , spacing 36
            , height fill

            -- , Border.widthEach { bottom = 0, left = 0, right = 1, top = 0 }
            , Background.color bgColor
            , Border.color (withAlpha 0.1 bdColor)
            ]
        |> el ([ height fill, width (px sidebarWidth) ] ++ sidebarAttr)


bdPages : Route -> List SidebarElement -> Palette -> List (Element msg)
bdPages currentRoute pages palette =
    List.map
        (\x ->
            case x of
                Unauthorized ->
                    none

                Logo ->
                    FeatherIcons.customIcon
                        [ Svg.polygon [ points "12 2 2 7 12 12 22 7 12 2", Svg.Attributes.fill Colors.purpleHex, Svg.Attributes.stroke Colors.purpleHex ] []

                        -- , Svg.polyline [ points "2 17 12 22 22 17" ] []
                        , Svg.polyline [ points "2 13 12 18 22 13 12 8", Svg.Attributes.fill Colors.purpleHex, Svg.Attributes.stroke Colors.purpleHex, Svg.Attributes.opacity "0.2" ] []
                        ]
                        |> FeatherIcons.withSize 26
                        |> FeatherIcons.withViewBox "0 0 26 26"
                        |> FeatherIcons.toHtml []
                        |> html
                        |> el [ moveDown 2 ]
                        |> el [ padding 8, Background.color Colors.white, Border.rounded 4, Border.glow (Colors.withAlpha 0.1 Colors.black) 0.5 ]

                Divider ->
                    el [ height (px 1), width fill, Background.color (withAlpha 0.1 darkness) ] none

                Spacer ->
                    el [ height fill ] none

                Page page ->
                    renderPage page currentRoute palette
        )
        pages


type SidebarElement
    = Page ( String, FeatherIcons.Icon, Route )
    | Logo
    | Spacer
    | Divider
    | Unauthorized


renderPage : ( String, FeatherIcons.Icon, Route ) -> Route -> Palette -> Element msg
renderPage ( title, icon, route ) currentRoute palette =
    let
        iconColor =
            if currentRoute == route then
                [ Font.color
                    Colors.purple
                ]

            else
                [ Font.color
                    darkGreen
                ]

        label =
            row []
                [ icon
                    |> FeatherIcons.withSize 24
                    |> FeatherIcons.withStrokeWidth 2
                    |> FeatherIcons.toHtml []
                    |> html
                    |> el iconColor
                ]
    in
    if currentRoute == route then
        el [ centerX, onRight (el [ height (px 44), moveUp 10, width (px 2), Background.color Colors.purple, moveRight 24 ] none) ] label

    else
        link
            [ -- , moveUp 1
              centerX
            ]
            { url = Gen.Route.toHref route
            , label = label
            }



-- COLORS


red : Color
red =
    rgb255 246 52 32


black : Color
black =
    rgb 0 0 0


darkness : Color
darkness =
    rgb255 25 25 27


textColor : Color
textColor =
    rgb255 51 63 78


white : Color
white =
    rgb 1 1 1


darkGreen : Color
darkGreen =
    rgb255 31 73 88


destructiveRed : Color
destructiveRed =
    rgb255 209 11 27


green : Color
green =
    rgb255 0 141 105


grayText : Color
grayText =
    rgb255 157 162 175


orange : Color
orange =
    rgb255 223 127 65


transparent : Color
transparent =
    withAlpha 0 white


withAlpha : Float -> Color -> Color
withAlpha alpha color =
    let
        rgbColor =
            toRgb color
    in
    rgba rgbColor.red rgbColor.green rgbColor.blue alpha


type alias Palette =
    { isDark : Bool
    , backgroundColor : Color
    , darkBackgroundColor : Color
    , accentColor : Color
    , darkAccentColor : Color
    , textColor : Color
    , darkTextColor : Color
    }


defaultPalette : Palette
defaultPalette =
    { isDark = False
    , backgroundColor = white
    , darkBackgroundColor = darkness
    , accentColor = rgb255 0 96 128
    , darkAccentColor = rgb255 25 148 190
    , textColor = withAlpha 0.8 darkness
    , darkTextColor = withAlpha 0.7 white
    }


gridIcon =
    [ Svg.rect [ Svg.Attributes.x "3", Svg.Attributes.y "3", Svg.Attributes.width "7", Svg.Attributes.height "7", Svg.Attributes.fill "currentColor" ] []
    , Svg.rect [ Svg.Attributes.x "14", Svg.Attributes.y "3", Svg.Attributes.width "7", Svg.Attributes.height "7", Svg.Attributes.fill "currentColor" ] []
    , Svg.rect [ Svg.Attributes.x "14", Svg.Attributes.y "14", Svg.Attributes.width "7", Svg.Attributes.height "7", Svg.Attributes.fill "currentColor" ] []
    , Svg.rect [ Svg.Attributes.x "3", Svg.Attributes.y "14", Svg.Attributes.width "7", Svg.Attributes.height "7", Svg.Attributes.fill "currentColor" ] []
    ]
        |> FeatherIcons.customIcon



-- ICONS


{-|

    UI.customIcon FeatherIcons.arrowLeft 24 UI.textColor

-}
customIcon : FeatherIcons.Icon -> Float -> Color -> Element msg
customIcon icon size iconColor =
    icon
        |> FeatherIcons.withSize size
        |> FeatherIcons.withStrokeWidth 2.4
        |> FeatherIcons.toHtml []
        |> html
        |> el [ Font.color iconColor ]


{-|

    UI.customIconWThickness FeatherIcons.arrowLeft 24 UI.textColor 2

-}
customIconWThickness : FeatherIcons.Icon -> Float -> Color -> Float -> Element msg
customIconWThickness icon size iconColor thickness =
    icon
        |> FeatherIcons.withSize size
        |> FeatherIcons.withStrokeWidth thickness
        |> FeatherIcons.toHtml []
        |> html
        |> el [ Font.color iconColor ]


colorAnimatingIcon : FeatherIcons.Icon -> Float -> Color -> Element msg
colorAnimatingIcon icon size iconColor =
    icon
        |> FeatherIcons.withSize size
        |> FeatherIcons.withStrokeWidth 2.4
        |> FeatherIcons.toHtml []
        |> html
        |> el
            [ Font.color iconColor
            , Border.rounded 100
            , mouseOver [ Background.color (withAlpha 0.1 black) ]
            , padding 12
            ]


raisedEl : List (Attribute msg) -> Element msg -> Element msg
raisedEl attr child =
    el
        ([ width (px 260)
         , Background.color white
         , Border.rounded 8
         , Border.width 1
         , Border.color (withAlpha 0.1 black)
         , Border.shadow
            { offset = ( 0, 6 )
            , size = 0
            , blur = 8
            , color = withAlpha 0.1 black
            }
         , paddingEach
            { top = 21
            , right = 24
            , bottom = 28
            , left = 24
            }
         ]
            ++ attr
        )
        child


wrapButtonChild : List (Attribute msg) -> Element msg -> Element msg
wrapButtonChild attr child =
    el
        ([ paddingXY 12 8
         , mouseOver
            [ Background.color (withAlpha 0.05 darkGreen)
            ]
         , Border.rounded 5
         , pointer
         ]
            ++ attr
        )
        child


showIf bool element =
    if bool then
        element

    else
        el [] none


centerWrap : Attribute msg
centerWrap =
    Html.Attributes.class "centerWrap"
        |> htmlAttribute


getSizeOfWindow : (Int -> Int -> msg) -> Cmd msg
getSizeOfWindow msg =
    Browser.Dom.getViewport
        |> Task.perform (\info -> msg (round info.scene.width) (round info.scene.height))
