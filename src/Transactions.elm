module Transactions exposing (CalculationResult, Transaction, TransactionType,
    recalculate, toCsv, transTypeToString)

import CurrentPositions as CP
import Missions as M
import Numeral
import Utils as U
import List
import List


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
    = Ignore
    | Sell
    | Buy
    | Hold


type alias CalculationResult =
    { transactions : List Transaction
    , cashPosition : CP.CashPosition
    , totalValue : Float
    , totalValueIgnored : Float
    , totalValueForTrades : Float
    }


recalculate : List CP.CurrentPosition -> CP.CashPosition -> List M.Mission -> List String -> CalculationResult
recalculate currentPositions cashPosition missions ignoredSymbols =
    let
        ignored item =
            List.member item.symbol ignoredSymbols

        notIgnored item =
            not (ignored item)
    in
    -- Calculate how much total value is held in Fidelity (stock + cash)
    -- and how much of that should be used to allocated to stocks that
    -- haven't been ignored, setting aside the cash that should be still
    -- held afterward
    let
        valueOfAllPositions = Debug.log "valueOfAllPositions" (
            currentPositions
                |> List.map .currentValue
                |> List.sum
                |> U.dec2)

        valueOfNonIgnoredPositions = Debug.log "valueOfNonIgnoredPositions" (
            currentPositions
                |> List.filter notIgnored
                |> List.map .currentValue
                |> List.sum
                |> U.dec2)

        valueOfIgnoredPositions = Debug.log "valueOfIgnoredPositions" (
            currentPositions
                |> List.filter ignored
                |> List.map .currentValue
                |> List.sum
                |> U.dec2)

        totalValue = Debug.log "totalValue" (
            U.dec2 <| cashPosition.startCash + cashPosition.addCash + valueOfAllPositions)

        totalValueNotIgnored = Debug.log "totalValueNotIgnored" (
            U.dec2 <| cashPosition.startCash + cashPosition.addCash  + valueOfNonIgnoredPositions)
        
        totalWeight = Debug.log "totalWeight" (
            missions |> List.map (\m -> Maybe.withDefault m.defaultWeight m.weight) |> List.sum)

        missionPositions = M.weightedPositions totalWeight

        -- Normalize the non-ignored mission positions so that if two
        -- missions have the same stock, that's merged into one target
        -- allocation percent; then, normalize so that all of the target
        -- allocation percents add up to 100%
        allNonIgnoredMissionPositions = Debug.log "allNonIgnoredMissionPositions" (
            missions
                |> List.map missionPositions
                |> List.concat
                |> List.filter notIgnored
                |> U.listMerge .symbol
                    (\mp1 mp2 -> { mp1 | allocationPercent = mp1.allocationPercent + mp2.allocationPercent }))

        totalAllocationPercent = Debug.log "totalAllocationPercent" (
            allNonIgnoredMissionPositions
                |> List.map .allocationPercent
                |> List.sum)

        -- totalAllocationPercentIncludingIgnored =
        --     missions
        --         |> List.map missionPositions
        --         |> List.concat
        --         |> List.map .allocationPercent
        --         |> List.sum

        scaleFactor = Debug.log "scaleFactor" (
            if totalAllocationPercent > 0.0 then
                100.0 / totalAllocationPercent

            else
                1.0)

        normalizedTargetMissionPositions = Debug.log "normalizedTargetMissionPositions" (
            allNonIgnoredMissionPositions
                |> List.map (\item -> { item | allocationPercent = item.allocationPercent * scaleFactor }))

        -- Create some "sell everything" transactions representing all current positions,
        -- this will be merged into the list of transactions coming from missions to decide
        -- what not to sell
        transactionsForCurrent = Debug.log "transactionsForCurrent" (
            currentPositions
                |> List.map
                    (\p ->
                        { symbol = p.symbol
                        , description = p.description
                        , price = p.lastPrice
                        , startShares = p.quantity
                        , startAmount = p.currentValue
                        , optimalAmount = if ignored p then p.currentValue else 0.0
                        , endShares = if ignored p then p.quantity else 0
                        , endAmount = if ignored p then p.currentValue else 0.0
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
                    ))

        -- Create some "sell everything" transactions representing every ignored
        -- position coming from any mission
        transactionsForIgnored = Debug.log "transactionsForIgnored" (
            missions
                |> List.map missionPositions
                |> List.concat
                |> List.filter ignored
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
                    ))

        -- Merge current plus ignored; keep the current position over the mission
        -- position whenever the same transaction appears.
        transactionsForCurrentPlusIgnored = Debug.log "transactionsForCurrentPlusIgnored" (
            List.concat [ transactionsForCurrent, transactionsForIgnored ]
                |> U.listMerge .symbol (\current _ -> current))

        -- Figure out how much cash is available for trading across all of the
        -- not-ignored mission positions
        desiredEndCash = Debug.log "desiredEndCash" (
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
                            -- 100.0 - (totalAllocationPercentIncludingIgnored / toFloat (List.length missions))
                    in
                    U.dec2 <| totalValue * foolCashAllocation

            else
                cashPosition.desiredEndCash)

        totalValueForTrades = Debug.log "totalValueForTrades" (
            U.dec2 <| totalValueNotIgnored - desiredEndCash)

        -- Create transactions representing the target mission positions
        transactionsForMissions = Debug.log "transactionsForMissions" (
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
                    ))

        -- Merge all of the transactions together; whenever there is a current position
        -- and a matching desired mission position, use the mission position as the
        -- transaction target, but remember the starting shares and amount
        transactionsForAll = Debug.log "transactionsForAll" (
            List.concat [ transactionsForCurrentPlusIgnored, transactionsForMissions ]
                |> U.listMerge .symbol
                    (\current mission ->
                        { mission
                            | startShares = current.startShares
                            , startAmount = current.startAmount
                        }
                    ))

        -- Due to rounding errors we may end up trying to spend more than we have.
        -- Adjust transactions as necessary until our end cash is >= the desired
        -- end cash.
        overage = Debug.log "overage" (
            (transactionsForAll
                |> List.filterMap (\t -> if ignored t then Nothing else Just t.endAmount)
                |> List.sum
                |> U.dec2
            ) - totalValueForTrades)

        adjustment = Debug.log "adjustment" (
            if overage <= 0 then
                Nothing

            else
                -- find any non-ignored stock with enough end shares to sell to cover the overage
                transactionsForAll
                    |> List.filter notIgnored

                    -- create tuple (transaction, number of shares required to cover, price per share)
                    |> List.map (\t -> ( t, ceiling (overage / t.price), t.price ))

                    -- filter to only stocks with enough shares to cover
                    |> List.filter (\( t, shares, _ ) -> t.endShares >= shares)
                    |> Debug.log "possible adjustments"

                    -- create tuple (transaction, number of shares required to cover, excess if shares modified)
                    |> List.map (\( t, shares, price ) -> ( t, shares, (toFloat shares * price) - overage ))

                    -- sort by excess, low to high (lowest = closest to the mark)
                    |> List.sortBy (\( _, _, excess ) -> excess)
                    |> Debug.log "sorted"

                    -- take the first one
                    |> List.head

                    -- create tuple (symbol, shares)
                    |> Maybe.map (\( t, shares, _ ) -> (t.symbol, shares)))

        adjusted =
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
            adjusted
                |> List.map
                    (\t ->
                        { transType =
                            if ignored t then
                                Ignore

                            else if t.startShares == t.endShares then
                                Hold

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
        -- cashRemaining =
        --     totalValueForTrades
        --         + desiredEndCash
        --         - (completeTransactions |> List.filterMap (\t -> if ignored t then Nothing else Just t.endAmount) |> List.sum)
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
    , totalValueIgnored = valueOfIgnoredPositions
    , totalValueForTrades = totalValueForTrades
    }


transTypeToString : TransactionType -> String
transTypeToString t =
    case t of
        Ignore ->
            "Ignore"

        Sell ->
            "Sell"

        Buy ->
            "Buy"

        Hold ->
            "Hold"


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