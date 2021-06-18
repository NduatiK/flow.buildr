module UI.Components.PaginatedCollection exposing (viewCustomGrid, viewCustomList, viewList, viewTable)

import Api.PaginatedEntries exposing (PaginatedEntry)
import Element exposing (..)
import Element.Input as Input
import List exposing (intersperse)
import UI
import UI.Text


viewList :
    { page : PaginatedEntry entry
    , header : Element msg
    , renderEntry : Int -> entry -> Element msg
    , onSelectPage : Int -> msg
    , emptyState : Element msg
    }
    -> Element msg
viewList { page, header, renderEntry, onSelectPage, emptyState } =
    viewCustomList []
        { page = page
        , header = header
        , renderEntry = renderEntry
        , onSelectPage = onSelectPage
        , raised = True
        , intersperse = none
        , emptyState = emptyState
        }


viewCustomGrid :
    List (Attribute msg)
    ->
        { page : PaginatedEntry entry
        , header : Element msg
        , renderEntry : Int -> entry -> Element msg
        , onSelectPage : Int -> msg
        , raised : Bool
        , emptyState : Element msg
        }
    -> Element msg
viewCustomGrid attr { page, header, renderEntry, onSelectPage, raised, emptyState } =
    wrap []
        { page = page
        , header = header
        , body =
            page.entries
                |> List.indexedMap renderEntry
                |> wrappedRow [ width fill, spacing 30, UI.centerWrap ]
        , onSelectPage = onSelectPage
        , raised = raised
        , emptyState = emptyState
        }


viewCustomList :
    List (Attribute msg)
    ->
        { page : PaginatedEntry entry
        , header : Element msg
        , renderEntry : Int -> entry -> Element msg
        , onSelectPage : Int -> msg
        , raised : Bool
        , intersperse : Element msg
        , emptyState : Element msg
        }
    -> Element msg
viewCustomList attr { page, header, renderEntry, onSelectPage, raised, intersperse, emptyState } =
    wrap
        [ paddingEach
            { bottom = 30
            , left = 50
            , right = 50
            , top = 45
            }
        ]
        { page = page
        , header = header
        , body =
            column
                ([ width fill, spacing 30 ] ++ attr)
                (page.entries
                    |> List.indexedMap renderEntry
                    |> List.intersperse intersperse
                )
        , onSelectPage = onSelectPage
        , raised = raised
        , emptyState = emptyState
        }


{-|

    PaginatedCollection.viewTable
        [ Border.width 1
        , Border.color (UI.withAlpha 0.3 UI.darkness)
        ]
        { header = el UI.Text.header1 (text "Users")
        , onSelectPage = SelectedPage
        , page = users
        , renderEntry =
            [ { header = el UI.Text.caption2 (text "Email")
              , width = fill
              , view =
                    \_ u ->
                        text u.email
              }
            ]
        , emptyState = none
        , raised = True
        }

-}
viewTable :
    List (Attribute msg)
    ->
        { page : PaginatedEntry entry
        , header : Element msg
        , renderEntry : List (IndexedColumn entry msg)
        , onSelectPage : Int -> msg
        , raised : Bool
        , emptyState : Element msg
        }
    -> Element msg
viewTable attr { page, header, renderEntry, onSelectPage, raised, emptyState } =
    wrap []
        { page = page
        , header = header
        , body =
            indexedTable ([ width fill, spacing 4, padding 8, UI.centerWrap ] ++ attr)
                { data = page.entries
                , columns = renderEntry
                }
        , onSelectPage = onSelectPage
        , raised = raised
        , emptyState = emptyState
        }


wrap :
    List (Attribute msg)
    ->
        { page : PaginatedEntry entry
        , header : Element msg
        , body : Element msg
        , onSelectPage : Int -> msg
        , raised : Bool
        , emptyState : Element msg
        }
    -> Element msg
wrap attr { raised, header, page, emptyState, body, onSelectPage } =
    (if raised then
        UI.raisedEl

     else
        el
    )
        ([ width fill
         , paddingEach
            { bottom = 30
            , left = 20
            , right = 20
            , top = 45
            }
         ]
            ++ attr
        )
    <|
        column [ width fill, spacing 30 ]
            [ header
            , if page.entries == [] then
                emptyState

              else
                body
            , renderFooter page onSelectPage
            ]


renderFooter page onSelectPage =
    let
        nextEnabled =
            page.pageNumber < page.totalPages

        prevEnabled =
            page.pageNumber > 1
    in
    row (width fill :: UI.Text.body)
        [ text (String.fromInt page.totalEntries ++ " items")
        , el [ width (shrink |> minimum 50) ] none
        , row [ centerX, spacing 7 ]
            [ text <|
                String.join " " <|
                    [ "Page"
                    , String.fromInt page.pageNumber
                    , "of"
                    , String.fromInt page.totalPages
                    ]
            ]
        , row (alignRight :: UI.Text.body)
            [ Input.button
                [ if prevEnabled then
                    alpha 1

                  else
                    alpha 0.3
                ]
                { label =
                    UI.wrapButtonChild [] (text "Prev")
                , onPress =
                    if prevEnabled then
                        Just (onSelectPage (page.pageNumber - 1))

                    else
                        Nothing
                }
            , Input.button
                [ if nextEnabled then
                    alpha 1

                  else
                    alpha 0.3
                ]
                { label =
                    UI.wrapButtonChild [] (text "Next")
                , onPress =
                    if nextEnabled then
                        Just (onSelectPage (page.pageNumber + 1))

                    else
                        Nothing
                }
            ]
        ]
