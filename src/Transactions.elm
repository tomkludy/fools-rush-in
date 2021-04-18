module Transactions exposing (CalculationResult, Transaction, TransactionType,
    recalculate, toCsv, transTypeToString)

import CurrentPositions as CP
import Missions as M
import Numeral
import Utils as U
import List
import Dict exposing (Dict)

type alias Transaction =
    { transType : TransactionType
    , symbol : String
    , description : String
    , numberOfShares : Int
    , value : Float
    , price : Float
    , startShares : Int
    , startAmount : Float
    , startPercent : Float
    , optimalAmount : Float
    , optimalPercent : Float
    , endShares : Int
    , endAmount : Float
    , endPercent : Float
    }


type TransactionType
    = Sell
    | Buy
    | Hold
    | None


type alias CalculationResult =
    { transactions : List Transaction
    , cashPosition : CP.CashPosition
    , totalValue : Float
    , totalValueAdjusted : Float
    , totalValueForTrades : Float
    }


recalculate : List CP.CurrentPosition -> CP.CashPosition -> List M.Mission -> Dict String (Maybe Int) -> CalculationResult
recalculate currentPositions cashPosition missions adjustedSymbols =
    let
        adjusted item =
            Dict.member item.symbol adjustedSymbols

        notAdjusted item =
            not (adjusted item)
        
        costOfAdjusted p =
            case Dict.get p.symbol adjustedSymbols of
                Just (Just num) -> p.lastPrice * toFloat num
                _ -> p.currentValue

        numOfAdjusted p =
            case Dict.get p.symbol adjustedSymbols of
                Just (Just num) -> num
                _ -> p.quantity
    in
    -- Calculate how much total value is held in Fidelity (stock + cash)
    -- and how much of that should be used to allocated to stocks that
    -- haven't been adjusted, setting aside the cash that should be still
    -- held afterward
    let

    -- type alias CurrentPosition =
    -- { account : String
    -- , symbol : String
    -- , description : String
    -- , quantity : Int
    -- , lastPrice : Float
    -- , currentValue : Float
    -- }
        normalizedCurrentPositions =
            currentPositions
                |> U.listMerge .symbol
                    (\a b ->
                        { account = "(multiple)"
                        , symbol = a.symbol
                        , description = a.description ++ " (multiple accounts)"
                        , quantity = a.quantity + b.quantity
                        , lastPrice = a.lastPrice
                        , currentValue = a.currentValue + b.currentValue
                        }
                    )
                --|> Debug.log "normalizedCurrentPositions"
        
        valueOfAllStartingPositions =
            currentPositions
                |> List.map .currentValue
                |> List.sum
                |> U.dec2
                --|> Debug.log "valueOfAllStartingPositions"

        -- valueOfNonAdjustedPositions =
        --     currentPositions
        --         |> List.filter notAdjusted
        --         |> List.map .currentValue
        --         |> List.sum
        --         |> U.dec2
        --         --|> Debug.log "valueOfNonAdjustedPositions"

        costOfAdjustedPositions =
            normalizedCurrentPositions
                |> List.filter adjusted
                |> List.map costOfAdjusted
                |> List.sum
                |> U.dec2
                --|> Debug.log "costOfAdjustedPositions"

        totalValue = --Debug.log "totalValue" <|
            U.dec2 <| cashPosition.startCash + cashPosition.addCash + valueOfAllStartingPositions

        totalValueForTradesAndReserve = --Debug.log "totalValueForTradesAndReserve" <|
            U.dec2 <| totalValue - costOfAdjustedPositions
        
        totalWeight =
            missions
                |> List.map (\m -> Maybe.withDefault m.defaultWeight m.weight)
                |> List.sum
                --|> Debug.log "totalWeight"

        missionPositions = M.weightedPositions totalWeight

        -- Normalize the non-adjusted mission positions so that if two
        -- missions have the same stock, that's merged into one target
        -- allocation percent; then, normalize so that all of the target
        -- allocation percents add up to 100%
        allNonAdjustedMissionPositions =
            missions
                |> List.map missionPositions
                |> List.concat
                |> List.filter notAdjusted
                |> U.listMerge .symbol
                    (\mp1 mp2 -> { mp1 | allocationPercent = mp1.allocationPercent + mp2.allocationPercent })
                --|> Debug.log "allNonAdjustedMissionPositions"

        totalAllocationPercent =
            allNonAdjustedMissionPositions
                |> List.map .allocationPercent
                |> List.sum
                --|> Debug.log "totalAllocationPercent"

        -- totalAllocationPercentIncludingAdjusted =
        --     missions
        --         |> List.map missionPositions
        --         |> List.concat
        --         |> List.map .allocationPercent
        --         |> List.sum

        scaleFactor = --Debug.log "scaleFactor" <|
            if totalAllocationPercent > 0.0 then
                100.0 / totalAllocationPercent
            else
                1.0

        normalizedTargetMissionPositions =
            allNonAdjustedMissionPositions
                |> List.map (\item -> { item | allocationPercent = item.allocationPercent * scaleFactor })
                --|> Debug.log "normalizedTargetMissionPositions"

        -- Create some "sell everything" transactions representing all current positions,
        -- this will be merged into the list of transactions coming from missions to decide
        -- what not to sell
        transactionsForCurrent =
            normalizedCurrentPositions
                |> List.map
                    (\p ->
                        { symbol = p.symbol
                        , description = p.description
                        , price = p.lastPrice
                        , startShares = p.quantity
                        , startAmount = p.currentValue
                        , optimalAmount = if adjusted p then costOfAdjusted p else 0.0
                        , endShares = if adjusted p then numOfAdjusted p else 0
                        , endAmount = if adjusted p then costOfAdjusted p else 0.0
                        }
                    )
                |> U.listMerge .symbol
                    (\a b ->
                        { a
                            | description = a.description ++ " (multiple accounts)"
                            , startShares = a.startShares + b.startShares
                            , startAmount = a.startAmount + b.startAmount
                            , optimalAmount = a.optimalAmount + b.optimalAmount
                            , endShares = a.endShares + b.endShares
                            , endAmount = a.endAmount + b.endAmount
                        }
                    )
                --|> Debug.log "transactionsForCurrent"

        -- Create some "sell everything" transactions representing every adjusted
        -- position coming from any mission
        transactionsForAdjusted =
            missions
                |> List.map missionPositions
                |> List.concat
                |> List.filter adjusted
                |> List.map
                    (\p ->
                        { symbol = p.symbol
                        , description = p.description
                        , price = p.currentPrice
                        , startShares = 0
                        , startAmount = 0.0
                        , optimalAmount = 0.0
                        , endShares = 0
                        , endAmount = 0.0
                        }
                    )
                --|> Debug.log "transactionsForAdjusted"

        -- Merge current plus adjusted; keep the current position over the mission
        -- position whenever the same transaction appears.
        transactionsForCurrentPlusAdjusted =
            List.concat [ transactionsForCurrent, transactionsForAdjusted ]
                |> U.listMerge .symbol (\current _ -> current)
                --|> Debug.log "transactionsForCurrentPlusAdjusted"

        -- Figure out how much cash is available for trading across all of the
        -- not-adjusted mission positions
        desiredEndCash = --Debug.log "desiredEndCash" <|
            if cashPosition.matchFool then
                -- Figure out the average cash reserve percentage across
                -- the followed missions and apply it to the total value
                -- in the Fidelity account
                if List.isEmpty missions
                then cashPosition.startCash + cashPosition.addCash
                else
                    let
                        foolCashAllocation = missions
                            |> List.map (M.weightedCashReservePercent totalWeight)
                            |> List.sum
                            -- 100.0 - (totalAllocationPercentIncludingAdjusted / toFloat (List.length missions))
                    in
                    U.dec2 <| totalValue * foolCashAllocation

            else
                cashPosition.desiredEndCash

        totalValueForTrades = --Debug.log "totalValueForTrades" <|
            U.dec2 <| max 0.0 (totalValueForTradesAndReserve - desiredEndCash)

        -- Create transactions representing the target mission positions
        transactionsForMissions =
            normalizedTargetMissionPositions
                |> List.map
                    (\p ->
                        let
                            optimalAmount =
                                U.dec2 <| p.allocationPercent * totalValueForTrades / 100.0

                            endShares =
                                round (optimalAmount / p.currentPrice)
                        in
                        { symbol = p.symbol
                        , description = p.description
                        , price = p.currentPrice
                        , startShares = 0
                        , startAmount = 0.0
                        , optimalAmount = optimalAmount
                        , endShares = endShares
                        , endAmount = U.dec2 <| toFloat endShares * p.currentPrice
                        }
                    )
                --|> Debug.log "transactionsForMissions"

        -- Merge all of the transactions together; whenever there is a current position
        -- and a matching desired mission position, use the mission position as the
        -- transaction target, but remember the starting shares and amount
        transactionsForAll =
            List.concat [ transactionsForCurrentPlusAdjusted, transactionsForMissions ]
                |> U.listMerge .symbol
                    (\current mission ->
                        { mission
                            | startShares = current.startShares
                            , startAmount = current.startAmount
                        }
                    )
                --|> Debug.log "transactionsForAll"

        -- Due to rounding errors we may end up trying to spend more than we have.
        -- Adjust transactions as necessary until our end cash is >= the desired
        -- end cash.
        overage = 
            (transactionsForAll
                |> List.filterMap (\t -> if adjusted t then Nothing else Just t.endAmount)
                |> List.sum
                |> U.dec2
            ) - totalValueForTrades
            --|> Debug.log "overage"

        adjustment = --Debug.log "adjustment" <|
            if overage <= 0 then
                Nothing

            else
                -- find any non-adjusted stock with enough end shares to sell to cover the overage
                transactionsForAll
                    |> List.filter notAdjusted

                    -- create tuple (transaction, number of shares required to cover, price per share)
                    |> List.map (\t -> ( t, ceiling (overage / t.price), t.price ))

                    -- filter to only stocks with enough shares to cover
                    |> List.filter (\( t, shares, _ ) -> t.endShares >= shares)
                    --|> Debug.log "possible adjustments"

                    -- create tuple (transaction, number of shares required to cover, excess if shares modified)
                    |> List.map (\( t, shares, price ) -> ( t, shares, (toFloat shares * price) - overage ))

                    -- sort by excess, low to high (lowest = closest to the mark)
                    |> List.sortBy (\( _, _, excess ) -> excess)
                    --|> Debug.log "sorted"

                    -- take the first one
                    |> List.head

                    -- create tuple (symbol, shares)
                    |> Maybe.map (\( t, shares, _ ) -> (t.symbol, shares))

        altered =
            case adjustment of
                Nothing ->
                    transactionsForAll

                Just (symbol, shares) ->
                    transactionsForAll
                        |> List.map
                            (\t ->
                                if t.symbol /= symbol then
                                    t

                                else
                                    { t
                                        | endShares = t.endShares - shares
                                        , endAmount = U.dec2 <| toFloat (t.endShares - shares) * t.price
                                        , description = t.description ++ " (adjusted by " ++ String.fromInt shares ++ ")"
                                    }
                            )

        -- Fill in the rest of the fields making a complete Transaction record
        completeTransactions =
            altered
                |> List.map
                    (\t ->
                        { transType =
                            if t.startShares == t.endShares then
                                if t.startShares == 0
                                then None
                                else Hold
                            else if t.startShares > t.endShares then
                                Sell
                            else
                                Buy
                        , symbol = t.symbol
                        , description = t.description
                        , numberOfShares = abs (t.startShares - t.endShares)
                        , value = U.dec2 <| toFloat (t.startShares - t.endShares) * t.price
                        , price = t.price
                        , startShares = t.startShares
                        , startAmount = t.startAmount
                        , startPercent = 100.0 * t.startAmount / totalValue
                        , optimalAmount = t.optimalAmount
                        , optimalPercent = 100.0 * t.optimalAmount / totalValue
                        , endShares = t.endShares
                        , endAmount = t.endAmount
                        , endPercent = 100.0 * t.endAmount / totalValue
                        }
                    )

        -- Determine the real cash remaining
        cashRemaining =
            totalValue
                - (completeTransactions |> List.map .endAmount |> List.sum)

        oldCash =
            cashPosition

        newCash =
            { oldCash | desiredEndCash = desiredEndCash, actualEndCash = cashRemaining }
    in
    { transactions = completeTransactions
    , cashPosition = newCash
    , totalValue = totalValue
    , totalValueAdjusted = costOfAdjustedPositions
    , totalValueForTrades = totalValueForTrades
    }


transTypeToString : TransactionType -> String
transTypeToString t =
    case t of
        Sell -> "Sell"
        Buy -> "Buy"
        Hold -> "Hold"
        None -> "-"


toCsv : CP.CashPosition -> List Transaction -> Float -> String
toCsv cash transactions totalValue =
    String.join "\n" <|
        List.map (String.join ",") <|
            [ [ U.toCsvField "Start cash"
              , U.toCsvField "Add cash"
              , U.toCsvField "End cash"
              , U.toCsvField "End cash %"
              ]
            , [ U.toCsvField <| Numeral.format "$0,0.00" cash.startCash
              , U.toCsvField <| Numeral.format "$0,0.00" cash.addCash
              , U.toCsvField <| Numeral.format "$0,0.00" cash.actualEndCash
              , U.toCsvField <| Numeral.format "0.0%" <| cash.actualEndCash / totalValue
              ]
            , []
            , [ U.toCsvField "Action"
              , U.toCsvField "Symbol"
              , U.toCsvField "Description"
              , U.toCsvField "# of Shares"
              , U.toCsvField "Trade Value"
              , U.toCsvField "Last Price"
              , U.toCsvField "Start Shares"
              , U.toCsvField "Start Amount"
              , U.toCsvField "Start %"
              , U.toCsvField "End Shares"
              , U.toCsvField "End Amount"
              , U.toCsvField "End %"
              , U.toCsvField "Optimal Amount"
              , U.toCsvField "Optimal %"
              ]
            ]
                ++ List.map
                    createCsvRow
                    transactions

createCsvRow : Transaction -> List String
createCsvRow t =
    [ U.toCsvField <| transTypeToString t.transType
                        , U.toCsvField t.symbol
                        , U.toCsvField t.description
                        , U.toCsvField <| String.fromInt t.numberOfShares
                        , U.toCsvField <| Numeral.format "$0,0.00" t.value
                        , U.toCsvField <| Numeral.format "$0,0.00" t.price
                        , U.toCsvField <| String.fromInt t.startShares
                        , U.toCsvField <| Numeral.format "$0,0.00" t.startAmount
                        , U.toCsvField <| Numeral.format "0.0%" <| t.startPercent / 100.0
                        , U.toCsvField <| String.fromInt t.endShares
                        , U.toCsvField <| Numeral.format "$0,0.00" t.endAmount
                        , U.toCsvField <| Numeral.format "0.0%" <| t.endPercent / 100.0
                        , U.toCsvField <| Numeral.format "$0,0.00" t.optimalAmount
                        , U.toCsvField <| Numeral.format "0.0%" <| t.optimalPercent / 100.0
                        ]