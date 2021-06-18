module UI.VisualEffects exposing (..)

import Element exposing (..)
import Svg
import Svg.Attributes


addGooFilter =
    inFront
        (el [ width (px 0), height (px 0) ] <|
            html <|
                Svg.svg [ Svg.Attributes.version "1.1", Svg.Attributes.viewBox "0 0 0 0" ]
                    [ Svg.defs
                        []
                        [ Svg.filter [ Svg.Attributes.id "goo" ]
                            [ Svg.feGaussianBlur
                                [ Svg.Attributes.in_ "SourceGraphic"
                                , Svg.Attributes.stdDeviation "8"
                                , Svg.Attributes.result "blur"
                                ]
                                []
                            , Svg.feColorMatrix
                                [ Svg.Attributes.in_ "blur"
                                , Svg.Attributes.mode "matrix"
                                , Svg.Attributes.values "1 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 18 -7"
                                , Svg.Attributes.result "goo"
                                ]
                                []
                            , Svg.feBlend
                                [ Svg.Attributes.in_ "SourceGraphic"
                                , Svg.Attributes.in2 "goo"
                                , Svg.Attributes.operator "atop"
                                ]
                                []
                            ]
                        ]
                    ]
        )
