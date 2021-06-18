module Pages.Home_ exposing (Model, Msg, page)

import Colors
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import FeatherIcons
import Gen.Params.Home_ exposing (Params)
import Material.Icons
import Material.Icons.Types
import MaterialIcons
import Page
import Request
import Shared
import UI
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.advanced
        { init = init req
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    { req : Request.With Params }


init : Request.With Params -> ( Model, Effect Msg )
init req =
    ( Model req, Effect.none )



-- UPDATE


type Msg
    = ReplaceMe


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        ReplaceMe ->
            ( model, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Homepage"
    , body =
        [ UI.layout model.req.route UI.defaultPalette <|
            row [ width fill, height fill ]
                [ column [ height fill, width (px 300), padding 32 ]
                    [ row [ width fill ]
                        [ el [ Font.size 20 ] <| text "Intro Call Flow"
                        , el [ alignRight ] <| UI.customIcon UI.gridIcon 20 Colors.grey
                        ]
                    , el [ height (px 30) ] none
                    , Input.text [ Font.size 14, Border.rounded 18, Border.color (Colors.withAlpha 0.4 Colors.grey), height (px 38), paddingXY 12 11 ]
                        { label = Input.labelHidden ""
                        , onChange = always ReplaceMe
                        , placeholder = Just (Input.placeholder [ Font.size 14, moveUp 1 ] (text "Search"))
                        , text = ""
                        }
                    , el [ height (px 30) ] none
                    , el [ Font.size 13, Font.medium, Font.color Colors.grey ] <| text "USED"
                    , el [ height (px 16) ] none
                    , row [ spacing 8 ] <|
                        List.map
                            (\( color, icon ) ->
                                el
                                    [ width (px 44)
                                    , height (px 44)
                                    , Border.rounded 22
                                    , Background.color color
                                    ]
                                    -- (el [ centerX, centerY ] (UI.customIcon icon 20 Colors.white))
                                    (MaterialIcons.material [ centerX, centerY ]
                                        { icon = icon
                                        , size = 20
                                        , color = Colors.white
                                        }
                                    )
                            )
                        <|
                            [ ( Colors.purple, Material.Icons.smartphone )
                            , ( Colors.blue, Material.Icons.message )
                            , ( Colors.green, Material.Icons.dialpad )
                            ]
                    , el [ height (px 20) ] none
                    , el [ Font.size 13, Font.medium, Font.color Colors.grey ] <| text "AVAILABLE"
                    , el [ height (px 16) ] none
                    , column [ spacing 12 ]
                        (List.map
                            (\( state, icon, message ) ->
                                row [ spacing 12 ] <|
                                    [ el
                                        [ width (px 44)
                                        , height (px 44)
                                        , Border.rounded 22
                                        , Background.color
                                            (case state of
                                                Active color ->
                                                    color

                                                Disabled ->
                                                    Colors.withAlpha 0.3 Colors.grey
                                            )
                                        ]
                                        -- (el [ centerX, centerY ] (UI.customIcon icon 20 Colors.white))
                                        (MaterialIcons.material [ centerX, centerY ]
                                            { icon = icon
                                            , size = 20
                                            , color =
                                                case state of
                                                    Active _ ->
                                                        Colors.white

                                                    Disabled ->
                                                        Colors.grey
                                            }
                                        )
                                    , el [ Font.size 15, Font.medium ] (text message)
                                    ]
                            )
                            [ ( Active Colors.purple, Material.Icons.smartphone, "Phone Call" )
                            , ( Active Colors.darkBlue, Material.Icons.access_time, "Wait" )
                            , ( Active Colors.blue, Material.Icons.message, "Say" )
                            , ( Active Colors.teal, Material.Icons.alt_route, "Redirect" )
                            , ( Active Colors.green, Material.Icons.dialpad, "Phone Keyboard" )
                            , ( Active Colors.lime, Material.Icons.record_voice_over, "Record Call Audio" )
                            , ( Disabled, Material.Icons.translate, "Translate" )
                            , ( Disabled, Material.Icons.exit_to_app, "Unsubscribe from Group" )
                            ]
                        )
                    , el
                        [ width (px 56)
                        , height (px 56)
                        , Border.rounded 56
                        , Background.color Colors.purple
                        , alignBottom
                        , alignRight
                        , Border.shadow
                            { offset = ( 0, 4 )
                            , size = 4
                            , blur = 4 * 2
                            , color = Colors.withAlpha 0.3 Colors.purple
                            }
                        ]
                        -- (el [ centerX, centerY ] (UI.customIcon icon 20 Colors.white))
                        (MaterialIcons.material [ centerX, centerY ]
                            { icon = Material.Icons.add
                            , size = 20
                            , color = Colors.white
                            }
                        )
                    ]
                , viewCanvas model
                ]
        ]
    }


type State
    = Active Color
    | Disabled


viewCanvas model =
    el
        [ height fill
        , width fill
        , behindContent
            (el
                [ height fill
                , width fill
                , Background.color Colors.lightGrey
                ]
                none
            )
        , behindContent
            (el
                [ height fill
                , width fill
                , alpha 0.4
                , Background.tiled "dist/DotGrid.png"
                ]
                none
            )
        , Border.innerShadow
            { offset = ( -2, 0 )
            , size = 0
            , blur = 2
            , color = Colors.black
            }
        , inFront (viewCanvasHeader model)
        ]
        none


viewCanvasHeader model =
    row [ padding 20, width fill, spacing 12 ]
        [ el
            [ width (px 48)
            , height (px 48)
            , Border.rounded 44
            , Background.color Colors.white
            , Border.shadow
                { offset = ( 0, 4 )
                , size = 4
                , blur = 4 * 2
                , color = Colors.withAlpha 0.05 Colors.black
                }
            ]
            -- (el [ centerX, centerY ] (UI.customIcon icon 20 Colors.white))
            (MaterialIcons.material [ centerX, centerY ]
                { icon = Material.Icons.rotate_left
                , size = 24
                , color = Colors.grey
                }
            )
        , el
            [ width (px 48)
            , height (px 48)
            , Border.rounded 44
            , Background.color Colors.white
            , Border.shadow
                { offset = ( 0, 4 )
                , size = 4
                , blur = 4 * 2
                , color = Colors.withAlpha 0.05 Colors.black
                }
            ]
            -- (el [ centerX, centerY ] (UI.customIcon icon 20 Colors.white))
            (MaterialIcons.material [ centerX, centerY ]
                { icon = Material.Icons.rotate_right
                , size = 24
                , color = Colors.grey
                }
            )
        , row
            [ centerY
            , spacing 8
            , paddingXY 16 8
            , Font.size 16
            , Font.color Colors.grey
            , Border.rounded 6
            , Background.color Colors.lightGrey
            , alignRight
            ]
            [ text "Export"
            , MaterialIcons.material [ centerX, centerY ]
                { icon = Material.Icons.share
                , size = 20
                , color = Colors.grey
                }
            ]
        , el
            [ centerY
            , Border.rounded 6
            , alignRight
            , Background.color Colors.grey
            , Border.shadow
                { offset = ( 0, 4 )
                , size = 4
                , blur = 4 * 2
                , color = Colors.withAlpha 0.05 Colors.black
                }
            ]
          <|
            el
                [ paddingXY 8 8
                , Border.rounded 6
                , Background.color (Colors.withAlpha 0.6 Colors.white)
                ]
            <|
                MaterialIcons.material [ centerX, centerY ]
                    { icon = Material.Icons.more_horiz
                    , size = 24
                    , color = Colors.withAlpha 0.4 Colors.black
                    }
        , row
            [ centerY
            , spacing 8
            , paddingXY 16 8
            , Font.size 16
            , Font.color Colors.white
            , Border.rounded 6
            , alignRight
            , Background.color Colors.purple
            , Border.shadow
                { offset = ( 0, 4 )
                , size = 4
                , blur = 4 * 2
                , color = Colors.withAlpha 0.05 Colors.black
                }
            ]
            [ text "Publish Flow"
            , MaterialIcons.material [ centerX, centerY ]
                { icon = Material.Icons.arrow_right
                , size = 24
                , color = Colors.white
                }
            ]
        ]
