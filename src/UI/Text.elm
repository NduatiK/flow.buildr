module UI.Text exposing (..)

import Element.Font exposing (..)
import UI exposing (..)


header1 =
    [ bold
    , size 20
    ]


body =
    [ regular
    , size 14
    ]


caption =
    [ regular
    , size 18
    , color UI.green
    ]


caption2 =
    [ regular
    , size 14
    , color (withAlpha 0.6 darkness)
    ]


header2 =
    [ bold
    , size 14
    , color UI.green
    ]
