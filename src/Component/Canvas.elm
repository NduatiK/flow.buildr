module Component.Canvas exposing
    ( Model
    , Msg(..)
    , init
    , renderCanvas
    , update
    )

-- import Html exposing (Html, div)
-- import Html.Attributes exposing (style)

import Color
import Colors
import Effect
import Element exposing (..)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Grid
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
        (Model model) =
            canvasModel
    in
    el
        [ width fill
        , height fill
        , pointer
        , inFront
            (viewChrome model)
        ]
    <|
        render model count width height


viewChrome model =
    column
        [ alignBottom
        , alignLeft
        , moveUp 30
        , moveRight 40
        , spacing 12
        ]
        [ el
            [ padding 6
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
                [ padding 2
                , Element.Border.rounded 2
                ]
                { label =
                    MaterialIcons.material [ centerX, centerY ]
                        { icon = Material.Icons.Round.zoom_out_map
                        , size = 22
                        , color = Colors.grey
                        }
                , onPress =
                    Just ResetZoom
                }
        , column
            [ Element.Background.color Colors.white
            , padding 6
            , spacing 6
            , Element.Border.rounded 6
            , Element.Border.shadow
                { offset = ( 0, 3 )
                , size = 1
                , blur = 6
                , color = Colors.withAlpha 0.2 Colors.black
                }
            ]
            [ Element.Input.button
                [ padding 2
                , Element.Border.rounded 2
                ]
                { label =
                    MaterialIcons.material [ centerX, centerY ]
                        { icon = Material.Icons.zoom_in
                        , size = 24
                        , color = Colors.grey
                        }
                , onPress =
                    if model.scale > 1.6 then
                        Nothing

                    else
                        Just ZoomIn
                }
            , el
                [ width (px 20)
                , centerX
                , Element.Background.color Colors.grey
                , height (px 1)
                ]
                none
            , Element.Input.button
                [ padding 2
                , Element.Border.rounded 2
                ]
                { label =
                    MaterialIcons.material [ centerX, centerY ]
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


render model count width height =
    column [] []
