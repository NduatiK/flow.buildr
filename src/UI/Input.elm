module UI.Input exposing (..)module UI.Input exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html.Attributes
import UI
import UI.Text


{-| -}
text :
    String
    -> List (Attribute msg)
    ->
        { onChange : String -> msg
        , text : String
        , placeholder : Maybe (Input.Placeholder msg)
        , label : Input.Label msg
        }
    -> Element msg
text fieldName response attr opts =
    column [ width fill, spacing 4 ]
        (Input.text (attr ++ Api.Errors.inputErrorAttributes fieldName response) opts
            :: Api.Errors.inputErrorLabels fieldName response
        )


{-| If spell checking is available, this input will be spellchecked.
-}
spellChecked :
    String
    -> Api.Data.Data x
    -> List (Attribute msg)
    ->
        { onChange : String -> msg
        , text : String
        , placeholder : Maybe (Input.Placeholder msg)
        , label : Input.Label msg
        }
    -> Element msg
spellChecked fieldName response attr opts =
    column [ width fill, spacing 4 ]
        (Input.spellChecked (attr ++ Api.Errors.inputErrorAttributes fieldName response) opts
            :: Api.Errors.inputErrorLabels fieldName response
        )


{-| -}
search :
    String
    -> Api.Data.Data x
    -> List (Attribute msg)
    ->
        { onChange : String -> msg
        , text : String
        , placeholder : Maybe (Input.Placeholder msg)
        , label : Input.Label msg
        }
    -> Element msg
search fieldName response attr opts =
    column [ width fill, spacing 4 ]
        (Input.search (attr ++ Api.Errors.inputErrorAttributes fieldName response) opts
            :: Api.Errors.inputErrorLabels fieldName response
        )


{-| A password input that allows the browser to autofill.

It's `newPassword` instead of just `password` because it gives the browser a hint on what type of password input it is.

A password takes all the arguments a normal `Input.text` would, and also **show**, which will remove the password mask (e.g. `****` vs `pass1234`)

-}
newPassword :
    String
    -> Api.Data.Data x
    -> List (Attribute msg)
    ->
        { onChange : String -> msg
        , text : String
        , placeholder : Maybe (Input.Placeholder msg)
        , label : Input.Label msg
        , show : Bool
        }
    -> Element msg
newPassword fieldName response attr opts =
    column [ width fill, spacing 4 ]
        (Input.newPassword (attr ++ Api.Errors.inputErrorAttributes fieldName response) opts
            :: Api.Errors.inputErrorLabels fieldName response
        )


{-| -}
currentPassword :
    String
    -> Api.Data.Data x
    -> List (Attribute msg)
    ->
        { onChange : String -> msg
        , text : String
        , placeholder : Maybe (Input.Placeholder msg)
        , label : Input.Label msg
        , show : Bool
        }
    -> Element msg
currentPassword fieldName response attr opts =
    column [ width fill, spacing 4 ]
        (Input.currentPassword (attr ++ Api.Errors.inputErrorAttributes fieldName response) opts
            :: Api.Errors.inputErrorLabels fieldName response
        )


{-| -}
username :
    String
    -> Api.Data.Data x
    -> List (Attribute msg)
    ->
        { onChange : String -> msg
        , text : String
        , placeholder : Maybe (Input.Placeholder msg)
        , label : Input.Label msg
        }
    -> Element msg
username fieldName response attr opts =
    column [ width fill, spacing 4 ]
        (Input.username (attr ++ Api.Errors.inputErrorAttributes fieldName response) opts
            :: Api.Errors.inputErrorLabels fieldName response
        )


{-| -}
email :
    String
    -> Api.Data.Data x
    -> List (Attribute msg)
    ->
        { onChange : String -> msg
        , text : String
        , placeholder : Maybe (Input.Placeholder msg)
        , label : Input.Label msg
        }
    -> Element msg
email fieldName response attr opts =
    column [ width fill, spacing 4 ]
        (Input.email (attr ++ Api.Errors.inputErrorAttributes fieldName response) opts
            :: Api.Errors.inputErrorLabels fieldName response
        )


{-| A multiline text input.

By default it will have a minimum height of one line and resize based on it's contents.

-}
multiline :
    String
    -> Api.Data.Data x
    -> List (Attribute msg)
    ->
        { onChange : String -> msg
        , text : String
        , placeholder : Maybe (Input.Placeholder msg)
        , label : Input.Label msg
        , spellcheck : Bool
        }
    -> Element msg
multiline fieldName response attr opts =
    column [ width fill, spacing 4 ]
        (Input.multiline (attr ++ Api.Errors.inputErrorAttributes fieldName response) opts
            :: Api.Errors.inputErrorLabels fieldName response
        )


defaultCheckbox : Bool -> Element msg
defaultCheckbox checked =
    Element.el
        [ htmlAttribute (Html.Attributes.class "focusable")
        , Element.width
            (Element.px 15)
        , Element.height (Element.px 15)
        , Font.color UI.white
        , Element.centerY
        , Font.size 9
        , Font.center
        , Border.rounded 3
        , Border.color <|
            if checked then
                Element.rgb (59 / 255) (153 / 255) (252 / 255)

            else
                Element.rgb (211 / 255) (211 / 255) (211 / 255)
        , Border.shadow
            { offset = ( 0, 0 )
            , blur = 1
            , size = 1
            , color =
                if checked then
                    Element.rgba (238 / 255) (238 / 255) (238 / 255) 0

                else
                    Element.rgb (238 / 255) (238 / 255) (238 / 255)
            }
        , Background.color <|
            if checked then
                UI.green

            else
                UI.white
        , Border.width <|
            if checked then
                0

            else
                1
        , Element.inFront
            (Element.el
                [ Border.color UI.white
                , Element.height (Element.px 6)
                , Element.width (Element.px 9)
                , Element.rotate (degrees -45)
                , Element.centerX
                , Element.centerY
                , Element.moveUp 1
                , Element.moveRight 0
                , Element.transparent (not checked)
                , Border.widthEach
                    { top = 0
                    , left = 2
                    , bottom = 2
                    , right = 0
                    }
                ]
                Element.none
            )
        ]
        Element.none


labelAbove : String -> Input.Label msg
labelAbove label =
    Input.labelAbove []
        (column []
            [ el (UI.Text.caption ++ [ Font.color UI.grayText ]) (Element.text label)
            , el [ height (px 4) ] none
            ]
        )
