module Component.Canvas exposing
    ( Model
    , Msg(..)
    , init
    , renderCanvas
    , update
    )

import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Advanced exposing (..)
import Canvas.Texture exposing (Texture)
import Color
import Colors
import Effect
import Element as E
import Element.Background
import Element.Border
import Element.Input
import Grid
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Material.Icons
import Material.Icons.Round
import MaterialIcons


type Model
    = Model InternalModel


type alias InternalModel =
    { scale : Float
    }


init : Model
init =
    Model
        { scale = 1
        }


centerX viewWidth =
    viewWidth / 2


centerY viewHeight =
    viewHeight / 2



--- UPDATE


type Msg
    = ZoomIn
    | ZoomOut
    | ResetZoom


update msg (Model model) =
    Tuple.mapFirst Model <|
        case msg of
            ZoomIn ->
                ( { model | scale = model.scale + 0.1 }
                , Effect.none
                )

            ZoomOut ->
                ( { model | scale = model.scale - 0.1 }
                , Effect.none
                )

            ResetZoom ->
                ( { model | scale = 1 }
                , Effect.none
                )



--- VIEW


renderCanvas viewWidth canvasModel { count, viewHeight } =
    let
        (Model ({ scale } as model)) =
            canvasModel

        width =
            toFloat viewWidth

        height =
            toFloat viewHeight
    in
    E.el
        [ E.width E.fill
        , E.height E.fill
        , E.pointer
        , E.inFront
            (viewChrome model)
        ]
    <|
        E.html <|
            -- Html.div
            --     [ Html.Attributes.style "display" "flex"
            --     , Html.Attributes.style "justify-content" "center"
            --     , Html.Attributes.style "align-items" "center"
            --     ]
            --     [
            Canvas.toHtml
                ( viewWidth, viewHeight )
                [ Html.Attributes.style "display" "flex"
                , Html.Attributes.style "cursor" "grab"
                ]
                (clearScreen model width height
                    ++ [ render model count width height
                       ]
                )


viewChrome model =
    E.column
        [ E.alignBottom
        , E.alignLeft
        , E.moveUp 30
        , E.moveRight 40
        , E.spacing 12
        ]
        [ E.el
            [ E.padding 6
            , Element.Border.rounded 6
            , Element.Background.color Colors.white
            , Element.Border.shadow
                { offset = ( 0, 3 )
                , size = 1
                , blur = 6
                , color = Colors.withAlpha 0.2 Colors.black
                }
            ]
          <|
            Element.Input.button
                [ E.padding 2
                , Element.Border.rounded 2
                ]
                { label =
                    MaterialIcons.material [ E.centerX, E.centerY ]
                        { icon = Material.Icons.Round.zoom_out_map
                        , size = 22
                        , color = Colors.grey
                        }
                , onPress =
                    Just ResetZoom
                }
        , E.column
            [ Element.Background.color Colors.white
            , E.padding 6
            , E.spacing 6
            , Element.Border.rounded 6
            , Element.Border.shadow
                { offset = ( 0, 3 )
                , size = 1
                , blur = 6
                , color = Colors.withAlpha 0.2 Colors.black
                }
            ]
            [ Element.Input.button
                [ E.padding 2
                , Element.Border.rounded 2
                ]
                { label =
                    MaterialIcons.material [ E.centerX, E.centerY ]
                        { icon = Material.Icons.add
                        , size = 24
                        , color = Colors.grey
                        }
                , onPress =
                    if model.scale > 1.6 then
                        Nothing

                    else
                        Just ZoomIn
                }
            , E.el
                [ E.width (E.px 20)
                , E.centerX
                , Element.Background.color Colors.grey
                , E.height (E.px 1)
                ]
                E.none
            , Element.Input.button
                [ E.padding 2
                , Element.Border.rounded 2
                ]
                { label =
                    MaterialIcons.material [ E.centerX, E.centerY ]
                        { icon = Material.Icons.zoom_out
                        , size = 24
                        , color = Colors.grey
                        }
                , onPress =
                    if model.scale < 0.7 then
                        Nothing

                    else
                        Just ZoomOut
                }
            ]
        ]


gridSize =
    24


gridSizef =
    toFloat gridSize


clearScreen : InternalModel -> Float -> Float -> List Renderable
clearScreen model width height =
    shapes
        [ fill (Color.rgb 244 243 246)
        ]
        [ rect ( 0, 0 ) width height
        ]
        :: clear ( 0, 0 ) width height
        :: Grid.fold2d
            { rows = ceiling ((width / model.scale) / gridSize)
            , cols = ceiling ((height / model.scale) / gridSize)
            }
            (renderItem model.scale)
            []


renderItem scale ( col, row ) lines =
    let
        time =
            1

        ( colf, rowf ) =
            ( toFloat col, toFloat row )

        ( x, y ) =
            ( rowf * gridSizef * scale
            , colf * gridSizef * scale
            )

        ( u, v ) =
            ( colf / (gridSizef - 1)
            , rowf / (gridSizef - 1)
            )
    in
    shapes
        [ fill Color.black
        ]
        [ rect ( x - 1.2 / 2, y - 1.2 / 2 ) 1.2 1.2 ]
        :: lines


render model count width height =
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
            , scale model.scale model.scale

            -- , rotate (degrees (count * 3))
            ]
        , fill (Color.hsl color 0.7 0.7)
        ]
        [ circle ( x, y ) size ]
