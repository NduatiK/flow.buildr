module UI.Components.PostEditor exposing (Msg(..), view)

import Api.Data exposing (Data(..))
import Api.Errors
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import FeatherIcons
import Html
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import UI
import UI.Element
import UI.Input
import UI.Text


type Msg
    = UpdatedBody String
    | UpdatedTitle String
    | UpdatedDescription String
    | MinimizeEditor
    | MaximizeEditor
    | SubmitPost
    | SubmitSavedPost


view : Bool -> { a | minimized : Bool, title : String, description : String, blogText : String, wordCount : Int, response : Data value } -> Element Msg
view isNew model =
    column [ width fill, Font.size 16 ]
        [ UI.showIf model.minimized <|
            UI.Input.multiline "title"
                model.response
                [ width fill
                , Font.size 18
                , Font.medium
                , centerX
                , Background.color UI.transparent
                , Border.rounded 8
                , Border.width 2
                , Border.color (UI.withAlpha 0.1 UI.black)
                ]
                { onChange = UpdatedTitle
                , text = model.title
                , placeholder = Nothing
                , label = UI.Input.labelAbove "Blog Title"
                , spellcheck = True
                }
        , UI.showIf (not model.minimized) <|
            el UI.Text.header1 (text model.title)
        , el [ height (px 16) ] none
        , UI.showIf model.minimized <|
            column [ width fill, spacing 4 ]
                [ UI.Input.multiline "description"
                    model.response
                    [ width fill
                    , height (px 80)
                    , Font.size 16
                    , Font.medium
                    , centerX
                    , Background.color UI.transparent
                    , Border.rounded 8
                    , Border.width 2
                    , Border.color (UI.withAlpha 0.1 UI.black)
                    ]
                    { onChange = UpdatedDescription
                    , text = model.description
                    , placeholder = Nothing
                    , label = UI.Input.labelAbove "Description"
                    , spellcheck = True
                    }
                , el UI.Text.caption2 (text "This is what will show up on Google searches")
                ]
        , UI.showIf model.minimized <|
            el [ height (px 16) ] none
        , column [ width fill, spacing 4 ]
            [ el
                [ width fill
                , centerX
                , inFront
                    (el
                        [ alignRight
                        , moveRight 64
                        , moveDown 36
                        , paddingEach
                            { top = 8
                            , right = 12
                            , bottom = 8
                            , left = 8
                            }
                        , Border.roundEach
                            { bottomLeft = 0
                            , bottomRight = 8
                            , topLeft = 0
                            , topRight = 8
                            }
                        , Border.widthEach
                            { bottom = 1
                            , right = 1
                            , left = 0
                            , top = 1
                            }
                        , Border.color (UI.withAlpha 0.1 UI.black)
                        , padding 8
                        , Background.color UI.white
                        ]
                     <|
                        Input.button
                            []
                            { label =
                                UI.customIcon
                                    (if model.minimized then
                                        FeatherIcons.maximize

                                     else
                                        FeatherIcons.minimize
                                    )
                                    24
                                    UI.green
                            , onPress =
                                if model.minimized then
                                    Just MaximizeEditor

                                else
                                    Just MinimizeEditor
                            }
                    )
                ]
              <|
                html
                    (Html.div
                        [ Html.Attributes.classList
                            [ ( "focus", not model.minimized )
                            , ( "has-error", Api.Errors.hasErrorForFieldNamed "body" model.response )
                            ]
                        ]
                        [ Html.input
                            [ Html.Attributes.id "md-editor"
                            , Html.Attributes.type_ "hidden"
                            , Html.Attributes.name "content"
                            , Html.Attributes.value model.blogText
                            ]
                            []
                        , Html.node "trix-editor"
                            [ Html.Attributes.attribute "input" "md-editor"
                            , Html.Events.on "trix-change"
                                (Decode.at [ "target", "innerHTML" ] Decode.string
                                    |> Decode.map UpdatedBody
                                )
                            ]
                            []
                        ]
                    )
            , column [ spacing 4 ] (Api.Errors.inputErrorLabels "body" model.response)
            , el (alignRight :: UI.Text.caption2)
                (text <|
                    String.join "" <|
                        [ String.fromInt model.wordCount, " words" ]
                )
            ]
        , el [ height (px 16) ] none
        , row [ spacing 8, width fill ]
            [ Input.button
                []
                { onPress = Nothing
                , label =
                    UI.wrapButtonChild
                        [ Font.medium
                        , Border.width 1
                        , Border.color (UI.withAlpha 0.3 UI.black)
                        ]
                        (text
                            (if isNew then
                                "Discard"

                             else
                                "Cancel"
                            )
                        )
                }
            , Input.button [ alignRight ]
                { label =
                    UI.wrapButtonChild
                        [ Font.medium
                        , Font.color UI.darkGreen
                        , Background.color (UI.withAlpha 0.2 UI.green)
                        , mouseOver
                            [ Font.color UI.darkGreen
                            , Background.color (UI.withAlpha 0.3 UI.green)
                            ]
                        ]
                    <|
                        case model.response of
                            NotAsked ->
                                text "Save"

                            Failure _ ->
                                text "Try Again"

                            Loading ->
                                UI.loader 16 UI.darkGreen
                                    |> el [ centerX, centerY ]

                            Success _ ->
                                row [ spacing 4 ]
                                    [ text "Done"
                                    , UI.customIcon FeatherIcons.check 24 UI.textColor
                                    ]
                , onPress = Just SubmitPost
                }
            ]
        ]
