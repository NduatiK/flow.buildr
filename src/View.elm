module View exposing (View, map, none, placeholder, toBrowserDocument)

import Browser
import Colors
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font


type alias View msg =
    { title : String
    , body : List (Element msg)
    }


placeholder : String -> View msg
placeholder str =
    { title = str
    , body = [ Element.text str ]
    }


none : View msg
none =
    placeholder ""


map : (a -> b) -> View a -> View b
map fn view =
    { title = view.title
    , body = List.map (Element.map fn) view.body
    }


toBrowserDocument : View msg -> Browser.Document msg
toBrowserDocument view =
    { title = view.title
    , body =
        [ Element.layoutWith
            { options =
                [ focusStyle
                    { borderColor = Nothing
                    , backgroundColor = Just <| rgb 0.1 0.1 0.1
                    , shadow = Nothing
                    }
                ]
            }
            [ Font.family
                [ Font.typeface "DM Sans"
                , Font.typeface "Inter"
                , Font.typeface "-apple-system"
                , Font.typeface "BlinkMacSystemFont"
                , Font.typeface "Segoe UI"
                , Font.typeface "Roboto"
                , Font.typeface "Oxygen"
                , Font.typeface "Ubuntu"
                , Font.typeface "Cantarell"
                , Font.typeface "Open Sans"
                , Font.typeface "Helvetica Neue"
                , Font.typeface "sans-serif"
                , Font.sansSerif
                ]
            , Background.color Colors.white
            , Font.color Colors.black
            ]
          <|
            column [ width fill, height fill ] view.body
        ]
    }
