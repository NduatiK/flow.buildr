module UI.Element exposing
    ( paragraphNewline
    , renderStatCard
    )

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes
import UI exposing (..)
import UI.Text


renderStatCard : ( String, String ) -> Element msg
renderStatCard ( title, value ) =
    el
        [ width (px 260)
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
        (column [ centerY, spacing 8 ]
            [ el [ Font.bold, Font.size 35 ] (text value)
            , el [ Font.size 16 ] (text title)
            ]
        )


paragraphNewline : List (Attribute msg) -> List (Element msg) -> Element msg
paragraphNewline attrs children =
    Element.paragraph
        (htmlAttribute (Html.Attributes.class "wrap")
            :: attrs
        )
        children
