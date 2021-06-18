module UI.Masonry exposing (viewMasonry)

import Array
import Element exposing (..)


type alias Position =
    Int


type alias Height =
    Int


type alias Masonry =
    List (List ( Position, Height ))


columnHeight : List ( Position, Height ) -> Height
columnHeight column =
    List.foldl (\( _, height ) total -> height + total) 0 column


columnsHeights : Masonry -> List Height
columnsHeights masonry =
    List.map columnHeight masonry


positionOfShortestHeight : List Height -> Position
positionOfShortestHeight listOfHeights =
    let
        helper itemPosition itemHeight accPosition =
            if itemHeight == (Maybe.withDefault 0 <| List.minimum listOfHeights) then
                itemPosition

            else
                accPosition
    in
    indexedFoldl helper 0 listOfHeights


indexedFoldl func acc_ list =
    list
        |> List.foldl
            (\elem ( counter, acc ) ->
                ( counter + 1, func counter elem acc )
            )
            ( 0, acc_ )
        |> Tuple.second


minimumHeightPosition : Masonry -> Position
minimumHeightPosition masonry =
    masonry |> columnsHeights |> positionOfShortestHeight


addItemToMasonry : Position -> Height -> Masonry -> Masonry
addItemToMasonry position height masonry =
    let
        minPosition =
            minimumHeightPosition masonry

        column =
            Maybe.withDefault [] <| Array.get minPosition (Array.fromList masonry)

        newColumn_ =
            ( position, height ) :: column
    in
    Array.toList <| Array.set minPosition newColumn_ (Array.fromList masonry)


fromItems : List Height -> Int -> Masonry
fromItems items columns =
    indexedFoldl addItemToMasonry (List.repeat columns []) items


viewMasonry :
    { items : List ( item, Float )
    , spacing : Int
    , padding : Int
    , viewItem : Position -> item -> Element msg
    , columns : Int
    , defaultHeight : Int
    }
    -> Element msg
viewMasonry args =
    let
        heights : List Height
        heights =
            args.items
                |> List.map
                    (\( _, itemHeight ) ->
                        if itemHeight > 0 then
                            truncate itemHeight

                        else
                            args.defaultHeight
                    )

        dataArray : Array.Array ( item, Float )
        dataArray =
            args.items
                |> Array.fromList
    in
    row [ width fill, spacing args.spacing, padding args.padding ] <|
        List.map
            (\masonryColumn ->
                column [ width fill, alignTop, spacing args.spacing ] <|
                    List.map
                        (\( position, height_ ) ->
                            Array.get position dataArray
                                |> Maybe.map
                                    (\( item, _ ) ->
                                        el [ width fill, height (px height_) ]
                                            (args.viewItem position item)
                                    )
                                |> Maybe.withDefault none
                        )
                        (List.reverse masonryColumn)
            )
            (List.reverse <| fromItems heights args.columns)
