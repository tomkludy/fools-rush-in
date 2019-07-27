module Missions exposing (Mission, MissionPosition, parseMission, addMission, removeMission)

import List.Extra as L
import Utils as U

type alias MissionPosition =
    { symbol : String
    , description : String
    , currentPrice : Float
    , allocationPercent : Float
    }

type alias Mission =
    { name : String
    , missionPositions : List MissionPosition
    }

parseMission : String -> Maybe Mission
parseMission input =
    let lines = String.lines input
    in
    case lines |> L.find (String.contains " PORTFOLIO") of
        Nothing -> Nothing
        Just line -> Just
            { name = (String.indexes " PORTFOLIO" line |> List.head |> Maybe.withDefault 0 |> String.slice 0) line
            , missionPositions =
                List.filterMap
                    (\l ->
                        if  (List.length (String.indexes "%" l) < 2) ||
                            (List.length (String.indexes "$" l) < 2)
                            then Nothing
                            else (parseTargetLine l)
                    ) lines
            }

addMission : Mission -> List Mission -> List Mission
addMission m list =
    m::(removeMission m.name list)

removeMission : String -> List Mission -> List Mission
removeMission name list =
    list |> List.filter (\m -> m.name /= name)

parseTargetLine : String -> Maybe MissionPosition
parseTargetLine line =
    let row = String.split "\t" line |> List.map String.trim
    in
    -- Navigator changed their table format around; it's the only one where
    -- the first column is not the company name, and doesn't end with (SYMBOL)
    -- in parenthesis.
    if String.endsWith ")" (List.head row |> Maybe.withDefault "")
        then parseOlderTargetLine row
        else parseNavigatorTargetLine row

parseOlderTargetLine : List String -> Maybe MissionPosition
parseOlderTargetLine row =
    -- First column is always "Company (SYMBOL)" though there may be other
    -- parenthesis in the company name eg "Company (XYZ) (SYMBOL)"
    let fstCol = U.column row 0
        lastParenIdx = fstCol
                        |> String.indexes "("
                        |> List.reverse
                        |> List.head
                        |> Maybe.withDefault 0
        symbol = String.slice (lastParenIdx + 1) -1 fstCol
        description = String.slice 0 lastParenIdx fstCol |> String.trim

    -- Current price is the first column with a leading dollar sign
        currentPrice = L.findIndex (String.startsWith "$") row
                        |> Maybe.withDefault 0
                        |> U.columnFloat row
        
    -- Allocation percent is the first column with a percent sign
        allocationPercent = L.findIndex (String.endsWith "%") row
                        |> Maybe.withDefault 0
                        |> U.columnFloat row
    in
    Just
    { symbol = symbol
    , description = description
    , currentPrice = currentPrice
    , allocationPercent = allocationPercent
    }

parseNavigatorTargetLine : List String -> Maybe MissionPosition
parseNavigatorTargetLine row =
    Just
    { symbol = U.column row 2
    , description = U.column row 1
    , currentPrice = U.columnFloat row 5
    , allocationPercent = U.columnFloat row 3
    }
