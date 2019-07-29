module Utils exposing (columnIdx, column, columnInt, columnFloat, listMerge, dec2, toCsvField)

import List.Extra as L
import Dict

columnIdx : String -> List String -> Maybe Int
columnIdx = L.elemIndex

column : List String -> Int -> String
column row col =
    L.getAt col row
        |> Maybe.withDefault ""

columnInt : List String -> Int -> Int
columnInt row col =
    column row col
        |> String.replace "," ""
        |> String.split "."
        |> List.head
        |> Maybe.withDefault ""
        |> String.toInt
        |> Maybe.withDefault 0

columnFloat : List String -> Int -> Float
columnFloat row col =
    column row col
        |> String.replace "$" ""
        |> String.replace "," ""
        |> String.replace "%" ""
        |> String.toFloat 
        |> Maybe.withDefault 0.0

listMerge : (a -> comparable) -> (a -> a -> a) -> List a -> List a
listMerge keyFunc combineFunc lists =
    lists
        |> List.map (\item -> (keyFunc item, item))
        |> List.foldl (\(key, value) dict ->
                case Dict.get key dict of
                    Nothing -> Dict.insert key value dict
                    Just exist -> Dict.insert key (combineFunc exist value) dict
            ) Dict.empty
        |> Dict.toList
        |> List.sortBy Tuple.first
        |> List.map Tuple.second

dec2 : Float -> Float
dec2 f = toFloat (round <| f * 100.0) / 100.0

toCsvField : String -> String
toCsvField s =
    let mustQuote = String.contains "\"" s || String.contains "," s
        quoted = String.replace "\"" "\"\"" s
    in
    if mustQuote then "\"" ++ quoted ++ "\"" else s
