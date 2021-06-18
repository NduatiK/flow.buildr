module Pages.Flow_buildr exposing (Model, Msg, page)

import Browser.Events
import Colors
import Component.Canvas
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import FeatherIcons
import Gen.Params.Flow_buildr exposing (Params)
import Html
import Html.Attributes
import Html.Events
import Html.Events.Extra.Mouse
import Json.Decode as Decode
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
    { req : Request.With Params
    , count : Float
    , viewWidth : Int
    , viewHeight : Int
    , pickedUpFlowAction : Maybe ( FlowAction, Path )
    }


init : Request.With Params -> ( Model, Effect Msg )
init req =
    ( { req = req
      , count = 0
      , viewWidth = 800
      , viewHeight = 800
      , pickedUpFlowAction = Nothing
      }
    , Effect.fromCmd (UI.getSizeOfWindow GotWindowSize)
    )


type FlowAction
    = FlowAction Int


type alias Path =
    { start : Location
    , current : Location
    }


type alias Location =
    ( Float, Float )



-- UPDATE


type Msg
    = NoOp
    | Frame Float
    | GotWindowSize Int Int
      ---
    | ClickedDownOnFlowAction FlowAction Location
    | MovedFlowActionTo FlowAction Path
    | ReleasedFlowAction


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Effect.none )

        Frame _ ->
            ( { model | count = model.count + 0.01 }, Effect.none )

        GotWindowSize w h ->
            ( { model
                | viewWidth = w
                , viewHeight = h
              }
            , Effect.none
            )

        ClickedDownOnFlowAction flowAction startLocation ->
            ( { model
                | pickedUpFlowAction = Just ( flowAction, Path startLocation startLocation )
              }
            , Effect.none
            )

        MovedFlowActionTo flowAction path ->
            ( { model
                | pickedUpFlowAction =
                    model.pickedUpFlowAction
                        |> Maybe.map (\_ -> ( flowAction, path ))
              }
            , Effect.none
            )

        ReleasedFlowAction ->
            -- ( model
            ( { model | pickedUpFlowAction = Nothing }
            , Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onResize GotWindowSize
        , case model.pickedUpFlowAction of
            Nothing ->
                Sub.none

            Just ( flowAction, old ) ->
                Browser.Events.onVisibilityChange
                    (\x ->
                        if x == Browser.Events.Hidden then
                            ReleasedFlowAction

                        else
                            NoOp
                    )
        , Browser.Events.onAnimationFrameDelta Frame
        ]



-- onTouch event tag =
--     eventDecoder
--         |> Decode.map
--             (\ev ->
--                 { message = tag ev
--                 , preventDefault = True
--                 , stopPropagation = True
--                 }
--             )
--         |> Html.Events.custom event
-- eventDecoder =
--     Decode.map2
--         (\event offset ->
--             { event = event
--             , targetOffset = offset
--             }
--         )
--         Touch.eventDecoder
--         offsetDecoder
-- VIEW


view : Model -> View Msg
view model =
    { title = "Homepage"
    , body =
        [ el
            ([ width fill
             , height fill
             ]
                ++ List.concat
                    [ case model.pickedUpFlowAction of
                        Nothing ->
                            []

                        Just ( flowAction, old ) ->
                            -- Browser.Events.onMouseMove MovedFlowActionTo
                            [ Html.Events.Extra.Mouse.onMove (.pagePos >> (\x -> { old | current = x }) >> MovedFlowActionTo flowAction)
                                |> htmlAttribute
                            , Html.Events.Extra.Mouse.onUp (always ReleasedFlowAction)
                                |> htmlAttribute
                            ]
                    ]
            )
          <|
            UI.layout model.req.route UI.defaultPalette <|
                row [ width fill, height fill ]
                    [ viewActionBar model
                    , viewCanvas model
                    ]
        ]
    }


actionBarWidth =
    300


viewActionBar model =
    column
        [ height fill
        , width (px actionBarWidth)
        , htmlAttribute (Html.Attributes.style "z-index" "2")
        , padding 32
        , behindContent
            (paragraph
                [ Font.size 12
                , alignBottom
                , padding 32
                ]
                [ text (Debug.toString model) ]
            )
        ]
        [ row [ width fill ]
            [ el [ Font.size 20 ] <| text "Intro Call Flow"
            , el [ alignRight ] <| UI.customIcon UI.gridIcon 20 Colors.grey
            ]
        , el [ height (px 30) ] none
        , Input.text [ Font.size 14, Border.rounded 18, Border.color (Colors.withAlpha 0.4 Colors.grey), height (px 38), paddingXY 12 11 ]
            { label = Input.labelHidden ""
            , onChange = always NoOp
            , placeholder = Just (Input.placeholder [ Font.size 14, moveUp 1 ] (text "Search"))
            , text = ""
            }
        , el [ height (px 30) ] none
        , el [ Font.size 13, Font.medium, Font.color Colors.grey ] <| text "USED"
        , el [ height (px 16) ] none
        , row
            [ spacing 8
            ]
          <|
            List.indexedMap
                (\i ( color, icon ) ->
                    let
                        ( ( moveRightDist, moveDownDist ), useFilter ) =
                            calculateOffset centerDist i model.pickedUpFlowAction

                        centerDist =
                            (54 - childSize) / 2

                        parentSize =
                            if useFilter then
                                58

                            else
                                54

                        childSize =
                            40
                    in
                    el
                        [ width (px 54)
                        , height (px 54)
                        ]
                    <|
                        el
                            [ width (px parentSize)
                            , height (px parentSize)
                            , Border.rounded 50
                            , Background.color color
                            , if not useFilter && Maybe.map Tuple.first model.pickedUpFlowAction == Just (FlowAction i) then
                                moveDown moveDownDist

                              else
                                transparent False
                            , if not useFilter && Maybe.map Tuple.first model.pickedUpFlowAction == Just (FlowAction i) then
                                moveRight moveRightDist

                              else
                                transparent False
                            , htmlAttribute
                                (Html.Attributes.style "filter"
                                    "url('#goo')"
                                )
                            , htmlAttribute
                                (Html.Attributes.style "-webkit-filter"
                                    "url('#goo')"
                                )
                            , htmlAttribute (Html.Attributes.style "-webkit-transition" "box-shadow 0.1s ease, transform 0.1s ease, width 0.1s ease,height 0.1s ease")
                            , htmlAttribute (Html.Attributes.style "transition" "box-shadow 0.1s ease, transform 0.2s ease, width 0.3s ease,height 0.3s ease")
                            , behindContent
                                (el
                                    [ width (px childSize)
                                    , height (px childSize)
                                    , if useFilter then
                                        moveDown moveDownDist

                                      else
                                        moveDown centerDist
                                    , if useFilter then
                                        moveRight moveRightDist

                                      else
                                        moveRight centerDist
                                    , if useFilter then
                                        Border.glow color 1

                                      else
                                        transparent False
                                    , Border.color color
                                    , Border.rounded 22
                                    , Background.color color

                                    -- , Background.color Colors.black
                                    , htmlAttribute (Html.Attributes.style "-webkit-transition" "transform 0.1s ease")
                                    , htmlAttribute (Html.Attributes.style "transition" "transform 0.2s ease")
                                    , htmlAttribute
                                        (Html.Attributes.style "filter"
                                            "url('#goo')"
                                        )
                                    , htmlAttribute
                                        (Html.Attributes.style "-webkit-filter"
                                            "url('#goo')"
                                        )
                                    ]
                                    none
                                )
                            , if model.pickedUpFlowAction == Nothing then
                                Html.Events.Extra.Mouse.onDown
                                    ((\x ->
                                        ( Tuple.first x.pagePos
                                          --  - Tuple.first x.offsetPos
                                        , Tuple.second x.pagePos
                                          -- - Tuple.second x.offsetPos
                                        )
                                     )
                                        >> ClickedDownOnFlowAction (FlowAction i)
                                    )
                                    |> htmlAttribute

                              else
                                transparent False
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
            (List.indexedMap
                (\i ( state, icon, message ) ->
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


type State
    = Active Color
    | Disabled


calculateOffset mids i pickedUpFlowAction =
    case pickedUpFlowAction of
        Just ( FlowAction selI, path ) ->
            let
                calcDelta fn =
                    let
                        offset =
                            fn path.current - fn path.start
                    in
                    ( abs offset > 50
                    , offset
                        + mids
                      -- - 14
                    )
            in
            if selI == i then
                case ( calcDelta Tuple.first, calcDelta Tuple.second ) of
                    ( ( _, x ), ( True, y ) ) ->
                        ( ( x, y ), False )

                    ( ( True, x ), ( _, y ) ) ->
                        ( ( x, y ), False )

                    ( ( _, x ), ( _, y ) ) ->
                        ( ( x / 1.5 + mids / 1.5, y / 1.5 + mids / 1.5 ), True )

            else
                ( ( mids, mids ), False )

        Nothing ->
            ( ( mids, mids ), False )


viewCanvas model =
    el
        [ height fill
        , width fill
        , htmlAttribute (Html.Attributes.style "z-index" "1")
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
                , Background.tiled "/dist/DotGrid.png"
                ]
                none
            )
        , Border.innerShadow
            { offset = ( -2, 0 )
            , size = 0
            , blur = 2
            , color = Colors.black
            }
        , inFront (Component.Canvas.renderCanvas (model.viewWidth - actionBarWidth - UI.sidebarWidth) model)
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
        , link [ alignRight ]
            { url = "#"
            , label =
                row
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
            }
        ]
