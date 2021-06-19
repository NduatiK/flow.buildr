module Pages.Flow_buildr exposing (Model, Msg, page)

import Browser.Events
import Colors
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
    , canvas : CanvasModel
    , tree : Node
    }


type alias CanvasModel =
    { scale : Float
    }


init : Request.With Params -> ( Model, Effect Msg )
init req =
    ( { req = req
      , count = 0
      , viewWidth = 800
      , viewHeight = 800
      , pickedUpFlowAction = Nothing
      , tree =
            Node (NodeAttr Colors.purple True)
                [ Node (NodeAttr Colors.green True) []
                , Node (NodeAttr Colors.green False)
                    [ Node (NodeAttr Colors.green True) []
                    , Node (NodeAttr Colors.green True) []
                    ]
                , Node (NodeAttr Colors.darkBlue True)
                    [ Node (NodeAttr Colors.green True) []
                    , Node (NodeAttr Colors.green True)
                        [ Node (NodeAttr Colors.green True) []
                        ]
                    , Node (NodeAttr Colors.grey True) []
                    , Node (NodeAttr Colors.grey False)
                        [ Node (NodeAttr Colors.darkBlue True)
                            [ Node (NodeAttr Colors.green True) []
                            , Node (NodeAttr Colors.green True)
                                [ Node (NodeAttr Colors.green True) []
                                ]
                            , Node (NodeAttr Colors.grey True) []
                            , Node (NodeAttr Colors.grey True) []
                            ]
                        , Node (NodeAttr Colors.blue True)
                            [ Node (NodeAttr Colors.green True) []
                            , Node (NodeAttr Colors.green True) []
                            , Node (NodeAttr Colors.grey True) [ Node (NodeAttr Colors.grey True) [ Node (NodeAttr Colors.grey True) [ Node (NodeAttr Colors.grey True) [ Node (NodeAttr Colors.grey True) [] ] ] ] ]
                            , Node (NodeAttr Colors.grey True) []
                            ]
                        ]
                    ]
                , Node (NodeAttr Colors.blue True)
                    [ Node (NodeAttr Colors.green True) []
                    , Node (NodeAttr Colors.green True) []
                    ]
                , Node (NodeAttr Colors.green True) []
                ]
      , canvas =
            { scale = 1
            }
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
    | ZoomIn
    | ZoomOut
    | ResetZoom
      ---
    | ClickedDownOnFlowAction FlowAction Location
    | MovedFlowActionTo FlowAction Path
    | ReleasedFlowAction


update : Msg -> Model -> ( Model, Effect Msg )
update msg ({ canvas } as model) =
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

        ZoomIn ->
            ( { model | canvas = { canvas | scale = canvas.scale + 0.1 } }
            , Effect.none
            )

        ZoomOut ->
            ( { model | canvas = { canvas | scale = canvas.scale - 0.1 } }
            , Effect.none
            )

        ResetZoom ->
            ( { model | canvas = { canvas | scale = 1 } }
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
    let
        canvasWidth =
            model.viewWidth - actionBarWidth - UI.sidebarWidth
    in
    el
        [ height (px model.viewHeight)
        , width (px canvasWidth)
        , htmlAttribute (Html.Attributes.style "z-index" "1")
        , inFront (viewCanvasHeader model)
        , inFront (viewChrome model.canvas)
        ]
    <|
        el
            [ height fill
            , width (px canvasWidth)
            , scrollbars
            ]
        <|
            el
                [ width fill
                , height fill

                -- , behindContent
                --     (el
                --         [ htmlAttribute (Html.Attributes.style "width" "100%")
                --         , height fill
                --         -- ,
                --         , Background.color Colors.orange
                --         ]
                --         none
                --     )
                , scale (max 1 model.canvas.scale)
                , htmlAttribute (Html.Attributes.style "-webkit-transition" "transform 0.1s ease, zoom 0.2s ease")
                , htmlAttribute (Html.Attributes.style "transition" "transform 0.1s ease, zoom 0.2s ease")
                ]
            <|
                viewHtmlTree_ (toFloat model.viewHeight * max 0 ((model.canvas.scale - 1) / 3)) model.tree


type Node
    = Node NodeAttr (List Node)


type alias NodeAttr =
    { color : Color
    , expanded : Bool
    }


numberOfChildren node =
    let
        a =
            numberOfChildren_ node
    in
    if a == 1 then
        a

    else
        a


numberOfChildren_ : Node -> Int
numberOfChildren_ (Node { expanded } children) =
    if not expanded || children == [] then
        1

    else
        children
            |> List.map numberOfChildren_
            |> List.sum


viewHtmlTree_ offset tree =
    el
        [ alignLeft
        , Background.tiled "dist/DotGrid.png"
        , htmlAttribute (Html.Attributes.style "width" "auto")
        , htmlAttribute (Html.Attributes.style "min-width" "100%")
        , height fill
        , moveDown offset
        , moveRight offset
        , htmlAttribute (Html.Attributes.style "-webkit-transition" "transform 0.1s ease, zoom 0.2s ease")
        , htmlAttribute (Html.Attributes.style "transition" "transform 0.1s ease, zoom 0.2s ease")
        , paddingEach
            { top = 90
            , left = 24
            , right = 24
            , bottom = 30
            }
        ]
    <|
        viewHtmlTree
            { hasParent = False
            , hasSibling = False
            , parentWidth = numberOfChildren tree
            , widthOfLeftSiblings = 0
            , siblingCount = 0
            , widthOfLeftSibling = 0
            }
            tree


siblingPadding =
    40


viewHtmlTree :
    { hasParent : Bool
    , hasSibling : Bool
    , parentWidth : Int
    , widthOfLeftSiblings : Int
    , widthOfLeftSibling : Int
    , siblingCount : Int
    }
    -> Node
    -> Element Msg
viewHtmlTree { hasParent, hasSibling, parentWidth, widthOfLeftSiblings, siblingCount } ((Node { expanded } children) as node) =
    let
        nodeWidth =
            numberOfChildren node

        locationRelativeToParent =
            toFloat widthOfLeftSiblings + (toFloat (nodeWidth + 1) / 2)

        onSide =
            if locationRelativeToParent == (toFloat (parentWidth + 1) / 2) then
                Center

            else if locationRelativeToParent < (toFloat (parentWidth + 1) / 2) then
                Left

            else
                Right

        label =
            String.fromInt nodeWidth
                ++ "-s"
                ++ String.fromInt widthOfLeftSiblings
                ++ "-p"
                ++ String.fromInt parentWidth
                ++ "("
                ++ String.fromFloat locationRelativeToParent
                ++ ")"
    in
    column
        [ height fill
        , width fill
        , centerX

        -- , Background.color bgcolor
        , moveUp 4

        -- , if not hasParent then
        --     behindContent
        --         (verticalLine 1000)
        --   else
        --     moveUp 4
        -- , spacing 60
        ]
        [ column [ centerX ]
            [ if hasParent && hasSibling then
                let
                    relativeDistToCenter_ =
                        locationRelativeToParent - (toFloat (parentWidth + 1) / 2)

                    distToCenter =
                        if relativeDistToCenter_ == 0 then
                            0

                        else if relativeDistToCenter_ < 0 then
                            -- onLeft
                            abs relativeDistToCenter_
                                * (siblingPadding - 4 - 4 + circleWidth)
                                - siblingPadding
                                - (if circleWidth > 100 then
                                    2.9 * circleWidth / 10

                                   else if circleWidth > 50 then
                                    1.6 * circleWidth / 10

                                   else
                                    0
                                  )
                                + (if modBy 2 siblingCount == 0 then
                                    2

                                   else
                                    6.0
                                  )

                        else
                            -- onRight
                            relativeDistToCenter_
                                * (siblingPadding - 4 - 4 + circleWidth)
                                - siblingPadding
                in
                cornerToParent onSide (round distToCenter) []

              else
                none
            , circle
                [ inFront
                    -- (el [ centerX, centerY, Font.size 12 ] <|
                    --     text label
                    -- )
                    (MaterialIcons.material [ centerX, centerY ]
                        { icon = Material.Icons.rotate_left
                        , size = 24
                        , color = Colors.white
                        }
                    )
                ]
                node
            ]
        , case ( expanded, children ) of
            ( False, _ ) ->
                none

            ( _, [] ) ->
                none

            ( _, [ oneChild ] ) ->
                verticalLine 40

            _ ->
                column
                    [ centerX ]
                    [ verticalLine 40
                    , row [ centerX, spacing -4, moveUp 1 ]
                        [ cornerToChildren Left []
                        , cornerToChildren Right []
                        , cornerToChildren Center []
                        ]
                    ]
        , if not expanded then
            none

          else
            row [ centerX, spacing -4, moveUp 1 ] <|
                List.intersperse (el [ width (px siblingPadding) ] none) <|
                    List.reverse <|
                        (\( _, _, a ) -> a) <|
                            List.foldl
                                (\sibling ( leftSiblingsWidth, leftSiblingWidth, acc ) ->
                                    ( leftSiblingsWidth + numberOfChildren sibling
                                    , numberOfChildren sibling
                                    , viewHtmlTree
                                        { hasParent = True
                                        , hasSibling = List.length children > 1
                                        , parentWidth = nodeWidth
                                        , widthOfLeftSiblings = leftSiblingsWidth
                                        , widthOfLeftSibling = leftSiblingWidth
                                        , siblingCount = List.length children
                                        }
                                        sibling
                                        :: acc
                                    )
                                )
                                ( 0, 0, [] )
                                children

        -- [ column [ moveUp 4, moveRight 10 ]
        --     [ cornerDown Left []
        --     , circle [ moveLeft 24 ]
        --     ]
        -- , el [ width (px (280 - 4)) ] none
        -- , column [ moveUp 4 ]
        --     [ cornerDown Right []
        --     , circle [ moveRight 11 ]
        --     ]
        -- ]
        ]


verticalLine lineHeight =
    el
        [ width (px 4)
        , height (px lineHeight)
        , centerX
        , moveUp 1
        , Background.color Colors.orange
        ]
        none


type Side
    = Left
    | Right
    | Center


cornerOutWidth =
    18


cornerToChildren side attr =
    let
        color =
            Colors.orange
    in
    if side == Center then
        none

    else
        el
            ([ Border.color color
             , height (px 20)
             , width (px cornerOutWidth)
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

                        Center ->
                            [ width (px 4)
                            , centerX
                            , Background.color color
                            , Border.width 0
                            ]
                   )
                ++ attr
            )
            none


cornerToParentWidth =
    round ((circleWidth / 2) + 4)


cornerToParent side extensionWidth attr =
    let
        color =
            Colors.orange

        shapeHeight =
            40
    in
    el
        ([ Border.color color
         , height (px shapeHeight)
         , width (px cornerToParentWidth)
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
                        , alignRight
                        , onRight
                            (el
                                [ width (px (max 0 extensionWidth))
                                , Background.color color
                                , height (px 4)
                                , moveUp 4
                                , Border.roundEach
                                    { topLeft = 0
                                    , topRight = 0
                                    , bottomLeft = 0
                                    , bottomRight = 0
                                    }
                                ]
                                none
                            )
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
                        , moveLeft 12
                        , centerX
                        , onLeft
                            (el
                                [ width (px (max 0 (extensionWidth - 4)))
                                , Background.color color
                                , height (px 4)
                                , moveUp 4
                                , Border.roundEach
                                    { topLeft = 4
                                    , topRight = 0
                                    , bottomLeft = 4
                                    , bottomRight = 0
                                    }
                                ]
                                none
                            )
                        ]

                    Center ->
                        [ width (px 4)
                        , centerX
                        , above
                            (el
                                [ width (px 4)
                                , height (px 10)
                                , Background.color color
                                ]
                                none
                            )
                        , moveUp 1
                        , alignTop
                        , Background.color color
                        ]
               )
            ++ attr
        )
        none


circleWidth =
    64


circle attr (Node { expanded, color } _) =
    el
        ([ width (px circleWidth)
         , height (px circleWidth)
         , centerX
         , Border.rounded circleWidth
         , Border.width 4
         , Border.color
            (if expanded then
                color

             else
                Colors.orange
            )
         , if not expanded then
            below
                (column [ centerX, pointer ]
                    [ el [ width (px 2), centerX, height (px 10), Background.color Colors.orange ] none
                    , MaterialIcons.material
                        [ centerX
                        , alignBottom
                        , padding 4
                        , Background.color Colors.white
                        , Border.rounded 20
                        , Border.width 2
                        , Border.color Colors.orange
                        ]
                        { icon = Material.Icons.more_horiz
                        , size = 20
                        , color = Colors.orange
                        }
                    ]
                )

           else
            Background.color color
         , Background.color color
         , Border.shadow
            { offset = ( 0, 4 )
            , size = 4
            , blur = 4 * 2
            , color =
                Colors.withAlpha
                    (if expanded then
                        0.05

                     else
                        0.1
                    )
                    Colors.black
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
            , Border.rounded 6
            , Background.color Colors.white
            , Border.shadow
                { offset = ( 0, 3 )
                , size = 1
                , blur = 6
                , color = Colors.withAlpha 0.2 Colors.black
                }
            ]
          <|
            Input.button
                [ padding 2
                , Border.rounded 2
                ]
                { label =
                    MaterialIcons.material [ centerX, centerY ]
                        { icon = Material.Icons.zoom_out_map
                        , size = 22
                        , color = Colors.grey
                        }
                , onPress =
                    Just ResetZoom
                }
        , column
            [ Background.color Colors.white
            , padding 6
            , spacing 6
            , Border.rounded 6
            , Border.shadow
                { offset = ( 0, 3 )
                , size = 1
                , blur = 6
                , color = Colors.withAlpha 0.2 Colors.black
                }
            ]
            [ Input.button
                [ padding 2
                , Border.rounded 2
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
                , Background.color Colors.grey
                , height (px 1)
                ]
                none
            , Input.button
                [ padding 2
                , Border.rounded 2
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
