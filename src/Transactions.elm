module Transactions exposing (Transaction, TransactionType, CalculationResult, recalculate, transTypeToString, toCsv)

import CurrentPositions as CP
import Utils as U
import Missions as M
import Numeral

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
    , totalValueForTrades : Float
    }

recalculate : List CP.CurrentPosition -> CP.CashPosition -> List M.Mission -> List String -> CalculationResult
recalculate currentPositions cashPosition missions ignoredSymbols =
    let ignored item = List.member item.symbol ignoredSymbols
        notIgnored item = not (ignored item)
    in
    -- Calculate how much total value is held in Fidelity (stock + cash)
    -- and how much of that should be used to allocated to stocks that
    -- haven't been ignored, setting aside the cash that should be still
    -- held afterward
    let valueOfAllPositions =
                currentPositions
                    |> List.map (.currentValue)
                    |> List.sum
        valueOfNonIgnoredPositions =
                currentPositions
                    |> List.filter notIgnored
                    |> List.map (.currentValue)
                    |> List.sum
        totalValue = cashPosition.startCash + valueOfAllPositions
        totalValueNotIgnored = cashPosition.startCash + valueOfNonIgnoredPositions
    
    -- Normalize the non-ignored mission positions so that if two
    -- missions have the same stock, that's merged into one target
    -- allocation percent; then, normalize so that all of the target
    -- allocation percents add up to 100%
        allNonIgnoredMissionPositions =
                missions
                    |> List.map (.missionPositions)
                    |> List.concat
                    |> List.filter notIgnored
                    |> U.listMerge (.symbol)
                        (\mp1 mp2 -> { mp1 | allocationPercent = mp1.allocationPercent + mp2.allocationPercent })
        totalAllocationPercent =
                allNonIgnoredMissionPositions
                    |> List.map (.allocationPercent)
                    |> List.sum
        totalAllocationPercentIncludingIgnored =
                missions
                    |> List.map (.missionPositions)
                    |> List.concat
                    |> List.map (.allocationPercent)
                    |> List.sum

        scaleFactor = if (totalAllocationPercent > 0.0) then 100.0 / totalAllocationPercent else 1.0
        normalizedTargetMissionPositions =
                allNonIgnoredMissionPositions
                    |> List.map (\item -> { item | allocationPercent = item.allocationPercent * scaleFactor })
    
    -- Create some "sell everything" transactions representing all current positions,
    -- this will be merged into the list of transactions coming from missions to decide
    -- what not to sell
        transactionsForCurrent =
                currentPositions
                    |> List.map (\p ->
                        { symbol = p.symbol
                        , description = p.description
                        , price = p.lastPrice
                        , startShares = p.quantity
                        , startAmount = p.currentValue
                        , optimalAmount = 0.0
                        , endShares = 0
                        , endAmount = 0.0
                        })
                    |> U.listMerge (.symbol)
                        (\a b ->
                            { a | description = a.description ++ " (multiple accounts)"
                                , startShares = a.startShares + b.startShares
                                , startAmount = a.startAmount + b.startAmount
                            })
    
    -- Create some "sell everything" transactions representing every ignored
    -- position coming from any mission
        transactionsForIgnored =
                missions
                    |> List.map (.missionPositions)
                    |> List.concat
                    |> List.filter ignored
                    |> List.map (\p ->
                        { symbol = p.symbol
                        , description = p.description
                        , price = p.currentPrice
                        , startShares = 0
                        , startAmount = 0.0
                        , optimalAmount = 0.0
                        , endShares = 0
                        , endAmount = 0.0
                        })
    
    -- Merge current plus ignored; keep the current position over the mission
    -- position whenever the same transaction appears.
        transactionsForCurrentPlusIgnored =
                List.concat [transactionsForCurrent, transactionsForIgnored]
                    |> U.listMerge (.symbol) (\current mission -> current)

    -- Figure out how much cash is available for trading across all of the
    -- not-ignored mission positions
        desiredEndCash =
                if cashPosition.matchFool
                    then
                        -- Figure out the average cash reserve percentage across
                        -- the followed missions and apply it to the total value
                        -- in the Fidelity account
                        case List.isEmpty missions of
                            True -> totalValue
                            False ->
                                let numMissions = List.length missions
                                    foolCashAllocation = 100.0 - (totalAllocationPercentIncludingIgnored / toFloat (List.length missions))
                                in U.dec2 <| totalValue * foolCashAllocation / 100.0
                    else cashPosition.desiredEndCash
        totalValueForTrades = totalValueNotIgnored - desiredEndCash

    -- Create transactions representing the target mission positions
        transactionsForMissions =
                normalizedTargetMissionPositions
                    |> List.map (\p ->
                        let optimalAmount = U.dec2 <| p.allocationPercent * totalValueForTrades / 100.0
                            endShares = round (optimalAmount / p.currentPrice)
                        in
                        { symbol = p.symbol
                        , description = p.description
                        , price = p.currentPrice
                        , startShares = 0
                        , startAmount = 0.0
                        , optimalAmount = optimalAmount
                        , endShares = endShares
                        , endAmount = U.dec2 <| (toFloat endShares) * p.currentPrice
                        })

    -- Merge all of the transactions together; whenever there is a current position
    -- and a matching desired mission position, use the mission position as the
    -- transaction target, but remember the starting shares and amount
        transactionsForAll =
                List.concat [transactionsForCurrentPlusIgnored, transactionsForMissions]
                    |> U.listMerge (.symbol)
                        (\current mission ->
                            { mission
                                | startShares = current.startShares
                                , startAmount = current.startAmount
                            })
    
    -- Due to rounding errors we may end up trying to spend more than we have.
    -- Adjust transactions as necessary until our end cash is >= the desired
    -- end cash.
        spent = (transactionsForAll |> List.map (.endAmount) |> List.sum)
        overage = (transactionsForAll |> List.map (.endAmount) |> List.sum) - totalValueForTrades

        adjustment =
                if overage <= 0
                    then Nothing
                    else
                        transactionsForAll
                            |> List.filter (\t -> t.endShares > t.startShares)
                            |> List.filter (\t -> toFloat (t.endShares - t.startShares) * t.price >= overage)
                            |> List.map (\t -> (t.symbol, ceiling (overage / t.price), t.price))
                            |> List.map (\(symbol, shares, price) ->
                                 (symbol, shares, toFloat shares * price - overage))
                            |> List.sortWith (\(symbol1, shares1, error1) (symbol2, shares2, error2) ->
                                case compare (round <| error1 / 100.0) (round <| error2 / 100.0) of
                                    EQ -> compare shares1 shares2
                                    o -> o
                                )
                            |> List.head
        adjusted =
                case adjustment of
                    Nothing -> transactionsForAll
                    Just (symbol, shares, _) ->
                        transactionsForAll
                            |> List.map (\t ->
                                if t.symbol /= symbol
                                    then t
                                    else { t | endShares = t.endShares - shares
                                             , endAmount = U.dec2 <| toFloat (t.endShares - shares) * t.price
                                             , description = t.description ++ " (adjusted by " ++ (String.fromInt shares) ++ ")"
                                         }
                            )
    
    -- Fill in the rest of the fields making a complete Transaction record
        completeTransactions =
                adjusted
                    |> List.map (\t ->
                        { transType =
                                if ignored t
                                    then Ignore
                                    else if t.startShares == t.endShares
                                        then Hold
                                        else if t.startShares > t.endShares
                                            then Sell
                                            else Buy
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
                        })
        
    -- Determine the real cash remaining
        cashRemaining =
                totalValueForTrades + desiredEndCash -
                (completeTransactions |> List.map (.endAmount) |> List.sum)

        oldCash = cashPosition
        newCash = { oldCash | desiredEndCash = desiredEndCash, actualEndCash = cashRemaining }
    in
    { transactions = completeTransactions
    , cashPosition = newCash
    , totalValue = totalValue
    , totalValueForTrades = totalValueForTrades
    }

transTypeToString : TransactionType -> String
transTypeToString t =
    case t of
        Ignore -> "Ignore"
        Sell -> "Sell"
        Buy -> "Buy"
        Hold -> "Hold"

toCsv : CP.CashPosition -> List Transaction -> Float -> String
toCsv cash transactions totalValue =
    String.join "\n"
        <| List.map (String.join ",")
        <| [ [ U.toCsvField "Start cash"
             , U.toCsvField "End cash"
             , U.toCsvField "End cash %"
             ]
           , [ U.toCsvField <| Numeral.format "$0,0.00" cash.startCash
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
           ] ++ ( List.map (\t ->
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
           ) transactions )

