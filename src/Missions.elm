module Missions exposing (
    Mission, MissionPosition,
    addMission, parseMission, removeMission,
    setUseDefaultWeight, setWeight, weightedPositions, unweightedPositions,
    weightedCashReservePercent, unweightedCashReservePercent)

import List.Extra as L
import Utils as U
import Regex
import FloatInput exposing (FloatField)


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
    , defaultWeight : Float
    , weightInput : FloatField
    }

weightedCashReservePercent : Float -> Mission -> Float
weightedCashReservePercent totalWeight mission =
    let weight = Maybe.withDefault mission.defaultWeight mission.weight
    in
    (weight / totalWeight) * unweightedCashReservePercent mission

unweightedCashReservePercent : Mission -> Float
unweightedCashReservePercent mission =
    let (Private missionPositions) = mission.missionPositions
    in
    missionPositions
            |> List.map .allocationPercent
            |> List.sum
            |> \n -> 1.0 - (n / 100.0)
            |> max 0.0
            |> min 1.0

weightedPositions : Float -> Mission -> List MissionPosition
weightedPositions totalWeight mission =
    let
        (Private missionPositions) = mission.missionPositions
        weight = Maybe.withDefault mission.defaultWeight mission.weight / totalWeight
        weighted position = { position | allocationPercent = position.allocationPercent * weight }
    in
    List.map weighted missionPositions

unweightedPositions : Mission -> List MissionPosition
unweightedPositions mission =
    let
        (Private missionPositions) = mission.missionPositions
    in
    missionPositions

setUseDefaultWeight : Bool -> Mission -> Mission
setUseDefaultWeight useDefault mission =
    if useDefault
    then { mission | weight = Nothing, weightInput = FloatInput.fromFloat mission.defaultWeight }
    else { mission | weight = Just mission.defaultWeight }

setWeight : Float -> Mission -> Mission
setWeight value mission =
    { mission | weight = Just value }

portfolioNameRegex : Regex.Regex
portfolioNameRegex = 
    Maybe.withDefault Regex.never <|
        Regex.fromString "[^ ] (Portfolio|Performance)"

portfolioValueRegex : Regex.Regex
portfolioValueRegex =
    Maybe.withDefault Regex.never <|
        Regex.fromString "Total Value of Portfolio\\s+([^\\s]+)"

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

extractName : String -> String
extractName line =
    let
        slicer = List.concat [ String.indexes " Portfolio" line, String.indexes " Performance" line ]
            |> List.head
            |> Maybe.withDefault 0
            |> String.slice 0
    in slicer line

findJust : (a -> Maybe b) -> List a -> Maybe b
findJust predicate list =
    case list of
       [] -> Nothing
       item::remain -> case predicate item of
          Nothing -> findJust predicate remain
          Just result -> Just result

strToFloat : String -> Maybe Float
strToFloat str =
    str
        |> String.replace "$" ""
        |> String.replace "%" ""
        |> String.replace "," ""
        |> String.toFloat


parseMission : String -> Maybe Mission
parseMission input =
    let
        lines = String.lines input
        missionPositions = List.foldl keepPreviousLines [] lines
            |> List.reverse
            |> List.filterMap
                (\(a,b,l) ->
                    if (List.length (String.indexes "%" l) < 2)
                    || (List.length (String.indexes "$" l) < 2)
                    then Nothing
                    else parseTargetLine a b l
                )
        portfolioValue = lines
            |> findJust (Regex.findAtMost 1 portfolioValueRegex >> List.head)
            |> Maybe.andThen (.submatches >> Just)
            |> Maybe.andThen List.head
            |> Maybe.andThen identity
            |> Maybe.andThen strToFloat
            |> Maybe.withDefault 100.0
    in
    lines
        |> L.find (Regex.contains portfolioNameRegex)
        |> Maybe.andThen (Just << extractName)
        |> Maybe.andThen (\name -> Just
            { name = name
            , missionPositions = Private missionPositions
            , weight = Nothing
            , defaultWeight = portfolioValue
            , weightInput = FloatInput.fromFloat portfolioValue
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
        desc = a
            |> String.split "\t"
            |> L.last
            |> Maybe.withDefault ""
            |> String.trim
    in
    Just
        { symbol = symbol
        , description = desc
        , currentPrice = U.columnFloat row (List.length row - 3)
        , allocationPercent = U.columnFloat row 0
        }
