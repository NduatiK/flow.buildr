module Component.Canvas exposing (renderCanvas)

import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Advanced exposing (..)
import Color
import Element
import Html exposing (Html, div)
import Html.Attributes exposing (style)


centerX viewWidth =
    viewWidth / 2


centerY viewHeight =
    viewHeight / 2



-- renderCanvas : Model -> Element.Element Msg


renderCanvas viewWidth { count, viewHeight } =
    let
        width =
            toFloat viewWidth

        height =
            toFloat viewHeight
    in
    Element.html <|
        -- Html.div
        --     [ Html.Attributes.style "display" "flex"
        --     , Html.Attributes.style "justify-content" "center"
        --     , Html.Attributes.style "align-items" "center"
        --     ]
        --     [
        Canvas.toHtml
            ( viewWidth, viewHeight )
            [ Html.Attributes.style "display" "flex"
            ]
            [ clearScreen width height
            , render count width height
            ]



-- ]


clearScreen width height =
    shapes
        [ fill (Color.rgba 0 0 0 0)
        ]
        [ rect ( 0, 0 ) width height ]


render count width height =
    let
        size =
            100

        x =
            0

        -- -(size / 2)
        y =
            -(size / 2)

        color =
            toFloat (remainderBy 100 (round (count * 10)))
                / 100
                |> Debug.log "c"
    in
    shapes
        [ transform
            [ translate (centerX width) (centerY height)

            -- , rotate (degrees (count * 3))
            ]
        , fill (Color.hsl color 0.7 0.7)
        ]
        [ circle ( x, y ) size ]
