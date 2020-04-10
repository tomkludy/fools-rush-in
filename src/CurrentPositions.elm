module CurrentPositions exposing (
    CurrentPosition, CashPosition,
    initCurrentPositions, initCashPosition, parseCurrentPositions,
    setMatchFool, setPendingEndCash, commitPendingEndCash, revertPendingEndCash)

import Csv
import Utils as U

-- MODEL
type alias CurrentPosition =
    { account : String
    , symbol : String
    , description : String
    , quantity : Int
    , lastPrice : Float
    , currentValue : Float
    }

type alias CashPosition =
    { startCash : Float
    , desiredEndCash : Float
    , matchFool : Bool
    , pendingEndCash : Maybe Float
    , actualEndCash : Float
    }

initCurrentPositions : List CurrentPosition
initCurrentPositions = []

initCashPosition : CashPosition
initCashPosition =
    { startCash = 0.0
    , desiredEndCash = 0.0
    , matchFool = True
    , pendingEndCash = Nothing
    , actualEndCash = 0.0
    }

parseCurrentPositions : String -> Result String (List CurrentPosition, CashPosition)
parseCurrentPositions content =
    Csv.parse content
        |> Result.mapError Debug.toString
        |> Result.andThen mapCurrentPositions

setMatchFool : Bool -> CashPosition -> CashPosition
setMatchFool enable cp =
    { cp | matchFool = enable }

setPendingEndCash : String -> Float -> CashPosition -> CashPosition
setPendingEndCash value maximum cp =
    case value |> String.replace "$" "" |> String.replace "," "" |> String.toFloat of
        Nothing -> cp
        Just v -> { cp | pendingEndCash = Just <| max 0 (min maximum v) }

commitPendingEndCash : CashPosition -> CashPosition
commitPendingEndCash cp =
    case cp.pendingEndCash of
        Nothing -> cp
        Just newValue -> { cp | desiredEndCash = newValue, pendingEndCash = Nothing }

revertPendingEndCash : CashPosition -> CashPosition
revertPendingEndCash cp =
    { cp | pendingEndCash = Nothing }


-- All internal from here down --

-- COLUMN NAMES IN FIDELITY FILE
findHeaders : List String -> Maybe HeaderIndexes
findHeaders headers =
    let midxs = [ U.columnIdx "Account Name/Number" headers
                , U.columnIdx "Symbol" headers
                , U.columnIdx "Description" headers
                , U.columnIdx "Quantity" headers
                , U.columnIdx "Last Price" headers
                , U.columnIdx "Current Value" headers
                ]
    in
    case midxs of
        (Just accountNameCol)::(Just symbolCol)::(Just descriptionCol)::(Just quantityCol)::(Just lastPriceCol)::(Just currentValueCol)::[] ->
            Just
                { accountNameCol = accountNameCol
                , symbolCol = symbolCol
                , descriptionCol = descriptionCol
                , quantityCol = quantityCol
                , lastPriceCol = lastPriceCol
                , currentValueCol = currentValueCol
                }
        _ -> Nothing


mapCurrentPositions : Csv.Csv -> Result String (List CurrentPosition, CashPosition)
mapCurrentPositions value =
    case findHeaders value.headers of
        Nothing -> Err "Can't find expected columns"
        Just headers -> Ok
            ( List.filterMap (parsePosition headers) value.records
            , parseCash headers value.records
            )

parsePosition : HeaderIndexes -> List String -> Maybe CurrentPosition
parsePosition headers row =
    let account = U.column row headers.accountNameCol
    in
    if String.startsWith "X" account then
        case U.column row headers.symbolCol of
            "FCASH**" -> Nothing
            "" -> Nothing
            symbol -> Just
                { account = account
                , symbol = symbol
                , description = U.column row headers.descriptionCol
                , quantity = U.columnInt row headers.quantityCol
                , lastPrice = U.columnFloat row headers.lastPriceCol
                , currentValue = U.columnFloat row headers.currentValueCol
                }
    else
        Nothing

parseCash : HeaderIndexes -> List (List String) -> CashPosition
parseCash headers rows =
    List.filterMap (\row ->
        case U.column row headers.symbolCol of
            "FCASH**" -> Just (U.columnFloat row headers.quantityCol)
            _ -> Nothing
    ) rows
        |> List.sum
        |> \res ->
            { startCash = res
            , desiredEndCash = res
            , actualEndCash = res
            , pendingEndCash = Nothing
            , matchFool = True
            }

type alias HeaderIndexes =
    { accountNameCol : Int
    , symbolCol : Int
    , descriptionCol : Int
    , quantityCol : Int
    , lastPriceCol : Int
    , currentValueCol : Int
    }

