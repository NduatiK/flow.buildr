module Pages.Flow_buildr exposing (Model, Msg, page)

import Browser.Events
import Colors
import Component.Canvas as CanvasComponent
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
import Model.Actions as Actions exposing (Actions(..))
import Page
import Request
import Shared
import Svg
import Svg.Attributes
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
    , canvasState : CanvasComponent.Model
    }


init : Request.With Params -> ( Model, Effect Msg )
init req =
    ( { req = req
      , count = 0
      , viewWidth = 800
      , viewHeight = 800
      , pickedUpFlowAction = Nothing
      , canvasState = CanvasComponent.init
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
    | CanvasMsg CanvasComponent.Msg
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

        CanvasMsg msg_ ->
            let
                ( updatedCanvasState, canvasEffect ) =
                    CanvasComponent.update msg_ model.canvasState
                        |> Tuple.mapSecond (Effect.map CanvasMsg)

                ( updatedModel, effect ) =
                    updateWithCanvasMsg msg_ { model | canvasState = updatedCanvasState }
            in
            ( updatedModel
            , Effect.batch
                [ canvasEffect
                , effect
                ]
            )


updateWithCanvasMsg : CanvasComponent.Msg -> Model -> ( Model, Effect Msg )
updateWithCanvasMsg msg model =
    case msg of
        _ ->
            ( model, Effect.none )



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

        -- , Browser.Events.onAnimationFrameDelta Frame
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
        , Border.shadow
            { offset = ( 0, 0 )
            , size = 0
            , blur = 8
            , color = Colors.withAlpha 0.1 Colors.black
            }
        , behindContent
            (paragraph
                [ Font.size 12
                , alignBottom
                , padding 32
                ]
                [ text
                    (Debug.toString
                        { count = model.count
                        , pickedUpFlowAction = model.pickedUpFlowAction
                        , state = model.canvasState
                        }
                    )
                ]
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
                (renderDragableAction model 54)
                [ ( Colors.purple, Material.Icons.smartphone )
                , ( Colors.blue, Material.Icons.message )
                , ( Colors.green, Material.Icons.dialpad )
                ]
        , el [ height (px 20) ] none
        , el [ Font.size 13, Font.medium, Font.color Colors.grey ] <| text "AVAILABLE"
        , el [ height (px 16) ] none
        , column [ spacing 12 ]
            (List.indexedMap
                (\i action ->
                    let
                        { color, icon, title } =
                            Actions.config action
                    in
                    row [ spacing 12 ] <|
                        [ renderDragableAction model 44 (i + 3) ( color, icon )
                        , el [ Font.size 15, Font.medium ] (text title)
                        ]
                )
                [ Actions.PhoneCall
                , Actions.Wait
                , Actions.Say
                , Actions.Redirect
                , Actions.PhoneKeyboard
                , Actions.RecordCallAudio
                , Actions.Translate
                , Actions.UnsubscribeFromGroup
                , Actions.MakePayment
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


calculateOffset defaultSize mids i pickedUpFlowAction =
    case pickedUpFlowAction of
        Just ( FlowAction selI, path ) ->
            let
                calcDelta fn =
                    let
                        offset =
                            fn path.current - fn path.start
                    in
                    ( abs offset > defaultSize
                    , offset + mids
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


yellowEllipse =
    html <|
        Svg.svg [] <|
            [ Svg.circle
                [ Svg.Attributes.fill "rgba(240,240,10,0.5)"
                , Svg.Attributes.cx "60"
                , Svg.Attributes.cy "120"
                , Svg.Attributes.r "30"
                ]
                []
            ]


renderDragableAction model defaultSize i ( color, icon ) =
    let
        ( ( moveRightDist, moveDownDist ), useFilter ) =
            calculateOffset (toFloat defaultSize) centerDist i model.pickedUpFlowAction

        centerDist =
            (toFloat defaultSize - toFloat childSize) / 2

        ( parentSize, offset ) =
            if useFilter then
                ( defaultSize + 4, -2 )

            else
                ( defaultSize, 0 )

        childSize =
            round (toFloat defaultSize * 40 / 54)

        colorExploded =
            Element.toRgb color
    in
    el
        [ width (px defaultSize)
        , height (px defaultSize)
        , behindContent
            (el
                [ width (px defaultSize)
                , centerX
                , height (px (defaultSize - 2))
                , centerY
                , Border.width 2
                , Border.color Colors.grey
                , Background.color (Colors.withAlpha 0.05 Colors.black)
                , Border.dashed
                , Border.rounded 50
                ]
                none
            )
        , htmlAttribute (Html.Attributes.style "z-index" "10")
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
                moveDown offset
            , if not useFilter && Maybe.map Tuple.first model.pickedUpFlowAction == Just (FlowAction i) then
                moveRight moveRightDist

              else
                moveRight offset
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
                -- , inFront
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
                        Border.glow color 0
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
                Border.rounded 50
            ]
            -- (el [ centerX, centerY ] (UI.customIcon icon 20 Colors.white))
            (MaterialIcons.material [ centerX, centerY ]
                { icon = icon
                , size = 20
                , color = Colors.white
                }
            )


viewCanvas model =
    el
        [ height fill
        , width fill
        , paddingEach
            { top = 90
            , left = 24
            , right = 24
            , bottom = 30
            }
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
                , Background.tiled "dist/DotGrid.png"
                ]
                none
            )
        , inFront <|
            Element.map CanvasMsg <|
                CanvasComponent.renderCanvas (model.viewWidth - actionBarWidth - UI.sidebarWidth) model.canvasState model
        , inFront (viewCanvasHeader model)
        ]
        (column
            [ centerX

            -- , Background.color Colors.orange
            , height fill
            , width fill
            , spacing 60
            ]
            [ circle
                [ below <|
                    column
                        [ centerX ]
                        [ verticalLine
                        , row [ centerX, spacing -4, moveUp 1 ]
                            [ cornerOut Left []
                            , cornerOut Right []
                            ]
                        ]
                ]
            , row [ centerX, spacing -4, moveUp 1 ]
                [ column [ moveUp 4, moveRight 10 ]
                    [ cornerDown Left []
                    , circle [ moveLeft 24 ]
                    ]
                , el [ width (px (280 - 4)) ] none
                , column [ moveUp 4 ]
                    [ cornerDown Right []
                    , circle [ moveRight 11 ]
                    ]
                ]
            ]
        )


verticalLine =
    el
        [ width (px 4)
        , height (px 40)
        , centerX
        , moveUp 1
        , Background.color Colors.orange
        ]
        none


type Side
    = Left
    | Right


cornerOut side attr =
    el
        ([ Border.color Colors.orange
         , height (px 20)
         , width (px 140)
         ]
            ++ (case side of
                    Left ->
                        [ Border.roundEach
                            { topLeft = 0
                            , topRight = 0
                            , bottomLeft = 0
                            , bottomRight = 20
                            }
                        , Border.widthEach
                            { top = 0
                            , bottom = 4
                            , left = 0
                            , right = 4
                            }
                        ]

                    Right ->
                        [ Border.roundEach
                            { topLeft = 0
                            , topRight = 0
                            , bottomLeft = 20
                            , bottomRight = 0
                            }
                        , Border.widthEach
                            { top = 0
                            , bottom = 4
                            , left = 4
                            , right = 0
                            }
                        ]
               )
            ++ attr
        )
        none


cornerDown side attr =
    el
        ([ Border.color Colors.orange
         , height (px 40)
         , width (px 40)
         ]
            ++ (case side of
                    Left ->
                        [ Border.roundEach
                            { topLeft = 20
                            , topRight = 0
                            , bottomLeft = 0
                            , bottomRight = 0
                            }
                        , Border.widthEach
                            { top = 4
                            , bottom = 0
                            , left = 4
                            , right = 0
                            }
                        ]

                    Right ->
                        [ Border.roundEach
                            { topLeft = 0
                            , topRight = 20
                            , bottomLeft = 0
                            , bottomRight = 0
                            }
                        , Border.widthEach
                            { top = 4
                            , bottom = 0
                            , left = 0
                            , right = 4
                            }
                        ]
               )
            ++ attr
        )
        none


circle attr =
    el
        ([ width (px 54)
         , height (px 54)
         , centerX
         , Border.rounded 44
         , Background.color Colors.orange
         , Border.shadow
            { offset = ( 0, 4 )
            , size = 4
            , blur = 4 * 2
            , color = Colors.withAlpha 0.05 Colors.black
            }
         ]
            ++ attr
        )
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
