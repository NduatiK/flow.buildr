module Model.Actions exposing
    ( Actions(..)
    , Inputs(..)
    , config
    )

import Colors
import Element
import Material.Icons
import Material.Icons.Types


type Actions
    = PhoneCall
    | Wait
    | Say
    | Redirect
    | PhoneKeyboard
    | RecordCallAudio
    | Translate
    | UnsubscribeFromGroup
    | MakePayment


type Inputs
    = Text String


type alias Config msg =
    { icon : Material.Icons.Types.Icon msg
    , color : Element.Color
    , title : String
    , maxChildren : Int
    , minChildren : Int
    , defaultChildren : List Actions
    , inputs : List Inputs
    }


config action =
    case action of
        PhoneCall ->
            { icon = Material.Icons.phonelink_ring
            , color = Colors.purple
            , title = "Phone Call"
            , maxChildren = 3
            , minChildren = 0
            , defaultChildren = []
            , inputs =
                [ Text ""
                ]
            }

        Wait ->
            { icon = Material.Icons.access_time
            , color = Colors.darkBlue
            , title = "Wait"
            , maxChildren = 1
            , minChildren = 0
            , defaultChildren = []
            , inputs =
                [ Text ""
                ]
            }

        Say ->
            { icon = Material.Icons.message
            , color = Colors.blue
            , title = "Say"
            , maxChildren = 1
            , minChildren = 0
            , defaultChildren = []
            , inputs =
                [ Text ""
                ]
            }

        Redirect ->
            { icon = Material.Icons.alt_route
            , color = Colors.teal
            , title = "Redirect"
            , maxChildren = 1
            , minChildren = 0
            , defaultChildren = []
            , inputs =
                [ Text ""
                ]
            }

        PhoneKeyboard ->
            { icon = Material.Icons.dialpad
            , color = Colors.green
            , title = "Phone Keyboard"
            , maxChildren = 1
            , minChildren = 0
            , defaultChildren = []
            , inputs =
                [ Text ""
                ]
            }

        RecordCallAudio ->
            { icon = Material.Icons.record_voice_over
            , color = Colors.lime
            , title = "Record Call Audio"
            , maxChildren = 1
            , minChildren = 0
            , defaultChildren = []
            , inputs =
                [ Text ""
                ]
            }

        Translate ->
            { icon = Material.Icons.translate
            , color = Colors.orange
            , title = "Translate"
            , maxChildren = 1
            , minChildren = 0
            , defaultChildren = []
            , inputs =
                [ Text ""
                ]
            }

        UnsubscribeFromGroup ->
            { icon = Material.Icons.exit_to_app
            , color = Colors.grey
            , title = "Unsubscribe from Group"
            , maxChildren = 1
            , minChildren = 0
            , defaultChildren = []
            , inputs =
                [ Text ""
                ]
            }

        MakePayment ->
            { icon = Material.Icons.payment
            , color = Colors.darkGreen
            , title = "Trigger Payment"
            , maxChildren = 1
            , minChildren = 0
            , defaultChildren = []
            , inputs =
                [ Text ""
                ]
            }
