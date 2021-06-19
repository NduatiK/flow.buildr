module Component.Canvas exposing
    ( Model
    , Msg(..)
    , init
    , renderCanvas
    , subscriptions
    , update
    )

-- import Html exposing (Html, div)
-- import Html.Attributes exposing (style)

import Browser.Events
import Circle2d
import Colors
import Effect
import Element exposing (..)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Geometry.Svg
import Graph.Force as Force
import Graph.GraphFile as GF exposing (FlowGraph, GraphFile)
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Material.Icons
import Material.Icons.Round
import MaterialIcons
import Model.Actions as A
import Point2d
import Quantity
import Svg as S exposing (Svg)
import Svg.Attributes as SA
import Svg.Events as SE
import Svg.Keyed
import Time


type Model
    = Model InternalModel


type Animation
    = NoAnimation
    | ForceAnimation Force.State


type alias InternalModel =
    { scale : Float
    , graph : GraphFile
    , animation : Animation
    , timeList : List Time.Posix
    }


init : Model
init =
    Model
        { scale = 1
        , graph = GF.default
        , animation = NoAnimation
        , timeList = []
        }



--- UPDATE


type Msg
    = ZoomIn
    | ZoomOut
    | ResetZoom
      --
    | ForceTick Time.Posix


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

            ForceTick t ->
                (\x -> ( x, Effect.none )) <|
                    case model.animation of
                        ForceAnimation forceState ->
                            if Force.isCompleted forceState then
                                model |> stopAnimation

                            else
                                let
                                    ( newForceState, newGF_ ) =
                                        GF.forceTick forceState model.graph

                                    newGF =
                                        -- case m.selectedTool of
                                        --     Select (DraggingSelection { brushStart, vertexPositionsAtStart }) ->
                                        --         let
                                        --             delta =
                                        --                 Vector2d.from brushStart m.svgMousePosition
                                        --             newVertexPositions =
                                        --                 vertexPositionsAtStart
                                        --                     |> IntDict.toList
                                        --                     |> List.map (Tuple.mapSecond (Point2d.translateBy delta))
                                        --         in
                                        --         newGF_ |> GF.setVertexPositions newVertexPositions
                                        --     _ ->
                                        newGF_
                                in
                                { model
                                    | animation = ForceAnimation newForceState
                                    , timeList = t :: model.timeList |> List.take 42
                                }

                        -- |> setPresentWithoutrecording newGF
                        _ ->
                            model


stopAnimation : InternalModel -> InternalModel
stopAnimation m =
    { m | animation = NoAnimation }


subscriptions : Model -> Sub Msg
subscriptions (Model model) =
    Sub.batch
        []



-- else
--     Sub.none
--- VIEW


renderCanvas viewWidth canvasModel { count, viewHeight } =
    let
        (Model model) =
            canvasModel
    in
    el
        [ width fill
        , height fill
        , inFront
            (viewChrome model)
        ]
    <|
        mainSvg model viewWidth viewHeight


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


emptySvgElement : Svg msg
emptySvgElement =
    S.g [] []


debugSvgElement : Svg msg
debugSvgElement =
    S.g
        [ SA.width (String.fromInt 100)
        , SA.height (String.fromInt 100)
        , HA.style "background-color" "red"
        ]
        []


mainSvg : InternalModel -> Int -> Int -> Element Msg
mainSvg m w h =
    let
        transparentInteractionRect =
            S.rect
                [ SA.fillOpacity "0"

                -- , SA.x (String.fromFloat (Point2d.xCoordinate m.pan))
                -- , SA.y (String.fromFloat (Point2d.yCoordinate m.pan))
                -- , SA.width (String.fromFloat (toFloat m.windowSize.width / m.zoom))
                -- , SA.height (String.fromFloat (toFloat m.windowSize.height / m.zoom))
                -- , HE.onMouseDown MouseDownOnTransparentInteractionRect
                -- , HE.onMouseUp MouseUpOnTransparentInteractionRect
                ]
                []

        -- maybeBrushedSelector =
        --     case m.selectedTool of
        --         Select (BrushingForSelection { brushStart }) ->
        --             case m.selectedSelector of
        --                 RectSelector ->
        --                     Geometry.Svg.boundingBox2d
        --                         [ SA.stroke (Colors.toString Colors.selectorStroke)
        --                         , SA.strokeWidth "1"
        --                         , SA.strokeDasharray "1 2"
        --                         , SA.fill "none"
        --                         ]
        --                         (BoundingBox2d.from brushStart m.svgMousePosition)
        --                 LineSelector ->
        --                     Geometry.Svg.lineSegment2d
        --                         [ SA.stroke (Colors.toString Colors.selectorStroke)
        --                         , SA.strokeWidth "1"
        --                         , SA.strokeDasharray "1 2"
        --                         ]
        --                         (LineSegment2d.from brushStart m.svgMousePosition)
        --         _ ->
        --             emptySvgElement
        -- cursor =
        --     case m.selectedTool of
        --         Gravity _ ->
        --             "crosshair"
        --         Hand HandIdle ->
        --             "grab"
        --         Hand (Panning _) ->
        --             "grabbing"
        --         Draw _ ->
        --             "crosshair"
        --         Select _ ->
        --             "default"
        -- mainSvgWidth =
        --     m.windowSize.width
        -- mainSvgHeight =
        --     m.windowSize.height
        -- svgViewBoxFromPanAndZoom pan zoom =
        --     [ Point2d.xCoordinate pan
        --     , Point2d.yCoordinate pan
        --     , toFloat mainSvgWidth / zoom
        --     , toFloat mainSvgHeight / zoom
        --     ]
        --         |> List.map String.fromFloat
        --         |> List.intersperse " "
        --         |> String.concat
        gFToShow =
            --     case m.animation of
            --         TransitionAnimation { startGraph, endGraph, transitionState } ->
            --             GF.transitionGraphFile
            --                 (Animation.elapsedTimeRatio transitionState)
            --                 { start = startGraph
            --                 , end = endGraph
            --                 }
            --         _ ->
            --             present m
            m.graph

        svgViewBoxFromWidthAndHeight =
            [ -(toFloat w / 2)
            , -105 - (GF.actionDefaultVertexProp.radius / 2)
            , toFloat w
            , toFloat h
            ]
                |> List.map String.fromFloat
                |> List.intersperse " "
                |> String.concat
    in
    html <|
        S.svg
            [ -- , HA.style "cursor" cursor
              -- , HA.style "position" "absolute"
              SA.width (String.fromInt w)
            , SA.height (String.fromInt h)
            , SA.viewBox svgViewBoxFromWidthAndHeight

            -- , HA.style "background-color" (Colors.toString Colors.mainSvgBackground)
            -- , SA.viewBox (svgViewBoxFromPanAndZoom m.pan m.zoom)
            -- , SE.onMouseDown MouseDownOnMainSvg
            -- , HE.on "wheel" (JD.map WheelDeltaY wheelDeltaY)
            ]
            [ --     maybeGravityLines m.selectedTool gFToShow
              -- , viewMapScale m.zoom
              -- , viewHulls gFToShow
              -- , maybeBrushedEdge m.selectedTool m.svgMousePosition gFToShow
              -- , transparentInteractionRect
              -- , maybeHighlightsOnSelectedEdges m.selectedEdges gFToShow
              -- , maybeHighlightOnMouseOveredEdges m.highlightedEdges gFToShow
              -- , maybeHighlightsOnSelectedVertices m.selectedVertices gFToShow
              -- , maybeHighlightOnMouseOveredVertices m.highlightedVertices gFToShow
              -- , viewEdges gFToShow
              --  ,
              viewVertices gFToShow

            -- , maybeBrushedSelector
            -- , maybeRectAroundSelectedVertices m.selectedTool m.selectedVertices gFToShow
            -- , maybeViewGravityCenters m.selectedTool gFToShow
            ]


viewVertices : GraphFile -> Html Msg
viewVertices graphFile =
    let
        pin fixed radius =
            if fixed then
                Geometry.Svg.circle2d
                    [ SA.fill "red"
                    , SA.stroke "white"
                    ]
                    (Point2d.origin
                        |> Circle2d.withRadius
                            (Quantity.float (radius / 2))
                    )

            else
                emptySvgElement

        viewVertex { id, label } =
            let
                { x, y } =
                    Point2d.unwrap label.position

                -- ( labelAnchor, labelX, labelY ) =
                --     case label.labelPosition of
                --         GF.LabelTopLeft ->
                --             ( "end"
                --             , -label.radius - 4
                --             , -label.radius - 4
                --             )
                --         GF.LabelTop ->
                --             ( "middle"
                --             , 0
                --             , -label.radius - 4
                --             )
                --         GF.LabelTopRight ->
                --             ( "start"
                --             , label.radius + 4
                --             , -label.radius - 4
                --             )
                --         GF.LabelCenter ->
                --             ( "middle"
                --             , 0
                --             , 0.39 * label.labelSize
                --             )
                --         GF.LabelLeft ->
                --             ( "end"
                --             , -label.radius - 4
                --             , 0.39 * label.labelSize
                --             )
                --         GF.LabelRight ->
                --             ( "start"
                --             , label.radius + 4
                --             , 0.39 * label.labelSize
                --             )
                --         GF.LabelBottomLeft ->
                --             ( "end"
                --             , -label.radius - 4
                --             , label.radius + label.labelSize
                --             )
                --         GF.LabelBottom ->
                --             ( "middle"
                --             , 0
                --             , label.radius + label.labelSize
                --             )
                --         GF.LabelBottomRight ->
                --             ( "start"
                --             , label.radius + 4
                --             , label.radius + label.labelSize
                --             )
                vertexLabel =
                    -- if label.labelIsVisible then
                    --     S.text_
                    --         [ SA.fill (Colors.toString label.labelColor)
                    --         , SA.fontSize (String.fromFloat label.labelSize)
                    --         , SA.textAnchor labelAnchor
                    --         , SA.x (String.fromFloat labelX)
                    --         , SA.y (String.fromFloat labelY)
                    --         ]
                    --         [ S.text label.label
                    --         ]
                    -- else
                    emptySvgElement

                circleSvg : Color -> Float -> Svg msg
                circleSvg c r =
                    Geometry.Svg.circle2d
                        [ SA.fill (Colors.toString c) ]
                        (Point2d.origin |> Circle2d.withRadius (Quantity.float r))

                backGroundCircleForBorder =
                    -- circleSvg label.borderColor label.radius
                    circleSvg Colors.orange label.radius

                innerCircle =
                    -- circleSvg label.color (label.radius - label.borderWidth)
                    circleSvg label.color label.radius
            in
            ( String.fromInt id
            , S.g
                [ SA.transform <| "translate(" ++ String.fromFloat x ++ "," ++ String.fromFloat y ++ ")"
                , SA.opacity (String.fromFloat 1)

                -- , SA.opacity (String.fromFloat label.opacity)
                -- , SE.onMouseDown (MouseDownOnVertex id)
                -- , SE.onMouseUp (MouseUpOnVertex id)
                -- , SE.onMouseOver (MouseOverVertex id)
                -- , SE.onMouseOut (MouseOutVertex id)
                ]
                [ backGroundCircleForBorder
                , innerCircle
                , pin label.fixed 10

                -- , pin label.fixed label.radius
                , vertexLabel
                ]
            )
    in
    Svg.Keyed.node "g" [] (graphFile |> GF.getVertices |> List.map viewVertex)
