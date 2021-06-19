module UI.VisualEffects exposing (..)

import Element
import Svg exposing (..)
import Svg.Attributes exposing (..)


addGooFilter : Element.Attribute msg
addGooFilter =
    Element.inFront
        (Element.el [ Element.width (Element.px 0), Element.height (Element.px 0) ] <|
            Element.html <|
                svg [ version "1.1", viewBox "0 0 0 0" ]
                    [ defs []
                        [ Svg.filter [ id "goo" ]
                            [ feGaussianBlur
                                [ in_ "SourceGraphic"
                                , stdDeviation "8"
                                , result "blur"
                                ]
                                []
                            , feColorMatrix
                                [ in_ "blur"
                                , mode "matrix"
                                , values "1 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 18 -7"
                                , result "goo"
                                ]
                                []
                            , feBlend
                                [ in_ "SourceGraphic"
                                , in2 "goo"
                                , operator "atop"
                                ]
                                []
                            ]
                        ]
                    ]
        )
