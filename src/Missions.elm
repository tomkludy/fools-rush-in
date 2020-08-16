module Missions exposing (
    Mission, MissionPosition,
    addMission, parseMission, removeMission,
    setUseDefaultWeight, setWeight, weightedPositions, unweightedPositions)

import List.Extra as L
import Utils as U
import Regex


type alias MissionPosition =
    { symbol : String
    , description : String
    , currentPrice : Float
    , allocationPercent : Float
    }


type Private = Private (List MissionPosition)

type alias Mission =
    { name : String
    , missionPositions : Private
    , weight : Maybe Float
    }

weightedPositions : Float -> Mission -> List MissionPosition
weightedPositions defaultWeight mission =
    let
        (Private missionPositions) = mission.missionPositions
        weight = Maybe.withDefault defaultWeight mission.weight / 100.0
        weighted position = { position | allocationPercent = position.allocationPercent * weight }
    in
    List.map weighted missionPositions

unweightedPositions : Mission -> List MissionPosition
unweightedPositions mission =
    let
        (Private missionPositions) = mission.missionPositions
    in
    missionPositions

setUseDefaultWeight : Float -> Bool -> Mission -> Mission
setUseDefaultWeight default useDefault mission =
    { mission | weight = if useDefault then Nothing else Just default }

setWeight : String -> Mission -> Mission
setWeight value mission =
    case value |> String.replace "$" "" |> String.replace "," "" |> String.toFloat of
        Nothing -> mission
        Just v -> { mission | weight = Just v }

portfolioNameRegex : Regex.Regex
portfolioNameRegex = 
    Maybe.withDefault Regex.never <|
        Regex.fromString "[^ ] (Portfolio|Performance)"

-- builds a list of lines in reverse order;
-- expects the head of "lines" to be the last line that was
-- processed, containing (line 2 before, line 1 before, itself)
keepPreviousLines : String -> List (String, String, String) -> List (String, String, String)
keepPreviousLines l lines =
    let
        (_, a, b) = lines
            |> List.head
            |> Maybe.withDefault ("", "", "")
    in
    (a, b, l) :: lines

parseMission : String -> Maybe Mission
parseMission input =
    let
        lines = String.lines input
    in
    lines
        |> L.find (\s -> Regex.contains portfolioNameRegex s)
        |> Maybe.map (\line ->
                { name = (
                        List.concat [ String.indexes " Portfolio" line, String.indexes " Performance" line ]
                            |> List.head
                            |> Maybe.withDefault 0
                            |> String.slice 0
                        ) line
                , missionPositions = Private (
                    List.foldl keepPreviousLines [] lines
                        |> List.reverse
                        |> List.filterMap
                            (\(a,b,l) ->
                                if (List.length (String.indexes "%" l) < 2)
                                || (List.length (String.indexes "$" l) < 2)
                                then Nothing
                                else parseTargetLine a b l
                            ))
                , weight = Nothing
                }
            )


addMission : Mission -> List Mission -> List Mission
addMission m list =
    m :: removeMission m.name list


removeMission : String -> List Mission -> List Mission
removeMission name list =
    list |> List.filter (\m -> m.name /= name)


parseTargetLine : String -> String -> String -> Maybe MissionPosition
parseTargetLine a b line =
    let
        row =
            String.split "\t" line |> List.map String.trim
        symbol = b
            |> String.trim
            |> String.split " "
            |> List.head
            |> Maybe.withDefault ""
    in
    Just
        { symbol = symbol
        , description = String.trim a
        , currentPrice = U.columnFloat row (List.length row - 3)
        , allocationPercent = U.columnFloat row 0
        }
