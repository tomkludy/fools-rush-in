module Main exposing ( main )
import Browser
import Html exposing (Html, button, div, text, img, p, strong)
import Html.Events exposing (onClick)
import Html.Attributes exposing (src, colspan, style)
import File exposing (File)
import File.Select as Select
import File.Download as Download
import Task
import Bootstrap.Table as Table
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Button as Button
import Bootstrap.Tab as Tab
import Bootstrap.Form.Textarea as Textarea
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Checkbox as Checkbox
import Numeral
import Toasty
import Toasty.Defaults
import CurrentPositions as CP
import Missions as M
import Transactions as T
import Json.Decode as D
import Port
import LocalStore as LS
import Utils as U
import FloatInput

-- MAIN
main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


-- MODEL
type alias Model =
    { currentPositions : List CP.CurrentPosition
    , cash : CP.CashPosition
    , missions : List M.Mission
    , transactions : List T.Transaction
    , tabState : Tab.State
    , toasties : Toasty.Stack Toasty.Defaults.Toast
    , ignoredSymbols : List String
    , totalValue : Float
    , totalValueIgnored : Float
    , totalValueForTrades : Float
    , addCashInput : FloatInput.FloatField
    , endCashInput : FloatInput.FloatField
    }

init : () -> (Model, Cmd Msg)
init _ =
    (
        { currentPositions = CP.initCurrentPositions
        , cash = CP.initCashPosition
        , missions = []
        , transactions = []
        , tabState = Tab.initialState
        , toasties = Toasty.initialState
        , ignoredSymbols = []
        , totalValue = 0.0
        , totalValueIgnored = 0.0
        , totalValueForTrades = 0.0
        , addCashInput = FloatInput.fromFloat 0.0
        , endCashInput = FloatInput.fromFloat 0.0
        }
    , LS.getLocalStore Port.IgnoredSymbols
    )



-- UPDATE

type Msg
    = CurrentPositionsRequested
    | CurrentPositionsSelected File
    | CurrentPositionsLoaded String
    | TabMsg Tab.State
    | AddTargetInputMsg String
    | ToastyMsg (Toasty.Msg Toasty.Defaults.Toast)
    | RemoveTargetRequested M.Mission
    | SwitchUseFoolAllocationMsg Bool
    | UpdatePendingTargetCashMsg String
    | UpdateTargetCashMsg
    | RevertTargetCashMsg
    | UpdateAddCashMsg String
    | SwitchUseDefaultWeightMsg Int Bool
    | UpdateWeightMsg Int String
    | ChangeIgnoreSymbolMsg String Bool
    -- | SendToJS Port.SendCommand
    | RecvFromJS D.Value
    | DownloadTransactionsMsg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        CurrentPositionsRequested ->
            ( model
            , Select.file ["text/csv"] CurrentPositionsSelected
            )
    
        CurrentPositionsSelected file ->
            ( model
            , Task.perform CurrentPositionsLoaded (File.toString file)
            )
        
        CurrentPositionsLoaded content ->
            case CP.parseCurrentPositions content of
                Err error -> ( model, Cmd.none ) |> addToastError error
                Ok positions ->
                    (let cash = Tuple.second positions
                     in { model
                        | currentPositions = Tuple.first positions
                        , cash = Tuple.second positions
                        , endCashInput = FloatInput.fromFloat cash.desiredEndCash
                      }
                    , Cmd.none
                    )
                        |> recalculate

        TabMsg state ->
            ( { model | tabState = state }
            , Cmd.none
            )
        
        AddTargetInputMsg input ->
            case M.parseMission input of
                Nothing -> (model, Cmd.none) |> addToastError "Does not appear to be a valid mission"
                Just mission ->
                    ( { model | missions = M.addMission mission model.missions }, Cmd.none )
                        |> recalculate
                        |> addToastSuccess ("Added mission: " ++ mission.name)
        
        ToastyMsg subMsg ->
            Toasty.update toastyConfig ToastyMsg subMsg model

        RemoveTargetRequested mission ->
            ( { model | missions = M.removeMission mission.name model.missions }
            , Cmd.none
            )
                |> recalculate
                |> addToastSuccess ("Removed mission: " ++ mission.name)

        SwitchUseFoolAllocationMsg enable ->
            ( { model | cash = CP.setMatchFool enable model.cash }, Cmd.none )
                |> recalculate

        UpdatePendingTargetCashMsg value ->
            ( FloatInput.updateModel value
                (\f -> { model | endCashInput = f })
                (\f m -> { m | cash = CP.setPendingEndCash f model.totalValueForTrades model.cash })
            , Cmd.none
            )
                |> recalculate

        UpdateAddCashMsg value ->
            ( FloatInput.updateModel value
                (\f -> { model | addCashInput = f })
                (\f m -> { m | cash = CP.setAddCash f model.cash })
            , Cmd.none
            )
                |> recalculate

        UpdateTargetCashMsg ->
            ( { model | cash = CP.commitPendingEndCash model.cash }, Cmd.none )
                |> recalculate

        RevertTargetCashMsg ->
            ( { model | cash = CP.revertPendingEndCash model.cash
                      , endCashInput = FloatInput.fromFloat model.cash.desiredEndCash }, Cmd.none )
                |> recalculate
        
        SwitchUseDefaultWeightMsg idx enable ->
            ( { model | missions = U.updateListItem idx model.missions
                <| M.setUseDefaultWeight enable
              }, Cmd.none )
                |> recalculate

        UpdateWeightMsg idx value ->
            ( { model | missions = U.updateListItem idx model.missions
                <|  (\mission -> FloatInput.updateModel value
                        (\f -> { mission | weightInput = f })
                        M.setWeight
                )
              }, Cmd.none )
                |> recalculate

        ChangeIgnoreSymbolMsg symbol ignore ->
            let newIgnored =
                    if ignore
                        then symbol::model.ignoredSymbols
                        else List.filter ((/=) symbol) model.ignoredSymbols
            in
            ( { model | ignoredSymbols = newIgnored }
            , LS.setLocalStore <| Port.IgnoredSymbolValue newIgnored
            )
                |> recalculate

        -- SendToJS cmd  ->
        --     case cmd of
        --         Port.SetLocalStore _ -> ( model, LS.setLocalStore <| Port.IgnoredSymbolValue model.ignoredSymbols )
        --         Port.GetLocalStore _ -> ( model, LS.getLocalStore Port.IgnoredSymbols )
        
        RecvFromJS incoming ->
            case LS.propRetrieved incoming of
                Err error -> ( model, Cmd.none ) |> addToastError error
                Ok (Just (Port.IgnoredSymbolValue list)) -> ( { model | ignoredSymbols = list }, Cmd.none ) |> recalculate
                Ok Nothing -> ( model, Cmd.none )
        
        DownloadTransactionsMsg ->
            ( model
            , T.toCsv model.cash model.transactions model.totalValue
                |> Download.string "transactions.csv" "text/csv"
            )


-- VIEW

view : Model -> Html Msg
view model =
    case List.length model.currentPositions of
        0 ->
            Grid.container []
                [ CDN.stylesheet
                , p [] [ text "Log into your Fidelity account and download your portfolio like so:" ]
                , img
                    [ src "fidelity-download-button.png"
                    , style "display" "block"
                    , style "width" "70%"
                    , style "margin" "auto"
                    , style "border" "5px"
                    , style "border-style" "outset"
                    ] []
                , p [] []
                , p [] [ text "Then click this button to open the portfolio and get started:" ]
                , Button.button [ Button.onClick CurrentPositionsRequested, Button.primary ] [ text "Load current positions" ]
                , Toasty.view toastyConfig Toasty.Defaults.view ToastyMsg model.toasties
                ]
        _ ->
            Grid.container []
                [ CDN.stylesheet
                , div [] [ text <| "Total value (cash + stock): " ++ Numeral.format "$0,0.00" model.totalValue ]
                , div [] [ text <| "Total reserved stock value (i.e. owned and ignored): " ++ Numeral.format "$0,0.00" model.totalValueIgnored ]
                , div [] [ text <| "Total value for trade (i.e. not ignored or reserved as cash): " ++ Numeral.format "$0,0.00" model.totalValueForTrades ]
                , Table.simpleTable
                    ( Table.simpleThead
                        [ Table.th [Table.cellPrimary] [ text "Current Cash" ]
                        , Table.th [Table.cellPrimary] [ text "Desired Cash" ]
                        , Table.th [Table.cellPrimary] [ text "Desired Cash %" ]
                        , Table.th [Table.cellPrimary] [ text "Actual End Cash" ]
                        , Table.th [Table.cellPrimary] [ text "Actual End Cash %" ]
                        ]
                    , Table.tbody []
                        [ Table.tr [] <|
                            Table.td []
                            [ text <| Numeral.format "$0,0.00" model.cash.startCash
                            , text " Add: "
                            , FloatInput.floatInput
                                "addCashInput"
                                model.addCashInput
                                UpdateAddCashMsg
                            ] :: (if List.isEmpty model.missions
                                then
                                    [ Table.td [ Table.cellAttr <| colspan 4 ] [ text "Add a Target mission to get started" ] ]
                                else
                                    [ Table.td [] (cashInput model)
                                    , Table.td [] [ text <| Numeral.format "0.0%" <| model.cash.desiredEndCash / model.totalValue ]
                                    , Table.td [] [ text <| Numeral.format "$0,0.00" model.cash.actualEndCash ]
                                    , Table.td [] [ text <| Numeral.format "0.0%" <| model.cash.actualEndCash / model.totalValue ]
                                    ]
                            )
                        ]
                    )
                , Tab.config TabMsg
                    |> Tab.withAnimation
                    |> Tab.items ((if List.isEmpty model.missions
                            then []
                            else List.concat
                                [ [transactionsTab model.ignoredSymbols model.transactions, currentPositionsTab model.currentPositions]
                                  , List.indexedMap (missionTab <| calcTotalWeight model.missions) model.missions
                                ]
                        ) ++ [addTargetTab])
                    |> Tab.view model.tabState
                , Toasty.view toastyConfig Toasty.Defaults.view ToastyMsg model.toasties
                ]

calcTotalWeight : List M.Mission -> Float
calcTotalWeight missions =
    missions
        |> List.map (\m -> Maybe.withDefault m.defaultWeight m.weight)
        |> List.sum

cashInput : Model -> List (Html.Html Msg)
cashInput model =
    let c = model.cash
    in
    List.concat 
        [   [ Checkbox.checkbox
                [ Checkbox.id "useFoolAllocationChk"
                , Checkbox.checked c.matchFool
                , Checkbox.onCheck SwitchUseFoolAllocationMsg
                ] "Use Fool allocation"
              ]
            , if c.matchFool
                then [ text (Numeral.format "$0,0.00" c.desiredEndCash) ]
                else case c.pendingEndCash of
                    Nothing ->
                        [ FloatInput.floatInput
                            "targetCashInput"
                            model.endCashInput
                            UpdatePendingTargetCashMsg
                        ]
                    _ ->
                        [ FloatInput.floatInput
                            "targetCashInput"
                            model.endCashInput
                            UpdatePendingTargetCashMsg
                        , Button.button [ Button.onClick UpdateTargetCashMsg, Button.primary ] [ text "Update" ]
                        , Button.button [ Button.onClick RevertTargetCashMsg, Button.primary ] [ text "Revert" ]
                        ]
        ]

transactionsTab : List String -> List T.Transaction -> Tab.Item Msg
transactionsTab ignoredSymbols cp =
    Tab.item
        { id = "tabTransactions"
        , link = Tab.link [] [ text "Transactions" ]
        , pane = Tab.pane []
            [ Button.button [ Button.onClick DownloadTransactionsMsg, Button.primary ] [ text "Download transactions as .csv file" ]
            , Table.simpleTable
                ( Table.simpleThead
                    [ Table.th [Table.cellPrimary] [ text "Ignore" ]
                    , Table.th [Table.cellPrimary] [ text "Action" ]
                    , Table.th [Table.cellPrimary] [ text "Symbol" ]
                    , Table.th [Table.cellPrimary] [ text "Description" ]
                    , Table.th [Table.cellPrimary] [ text "# of Shares" ]
                    , Table.th [Table.cellPrimary] [ text "Trade Value" ]
                    , Table.th [Table.cellPrimary] [ text "Last Price" ]
                    , Table.th [Table.cellPrimary] [ text "Start Shares" ]
                    , Table.th [Table.cellPrimary] [ text "Start Amount" ]
                    , Table.th [Table.cellPrimary] [ text "Start %" ]
                    , Table.th [Table.cellPrimary] [ text "End Shares" ]
                    , Table.th [Table.cellPrimary] [ text "End Amount" ]
                    , Table.th [Table.cellPrimary] [ text "End %" ]
                    , Table.th [Table.cellPrimary] [ text "Optimal Amount" ]
                    , Table.th [Table.cellPrimary] [ text "Optimal %" ]
                    ]
                , Table.tbody []
                    (cp |> List.map (\p ->
                        let isIgnored = List.member p.symbol ignoredSymbols
                        in
                        Table.tr []
                            [ Table.td [] [ Button.checkboxButton isIgnored [ Button.onClick (ChangeIgnoreSymbolMsg p.symbol (not isIgnored)) ] [] ]
                            , Table.td [] [ text <| T.transTypeToString p.transType ]
                            , Table.td [] [ text p.symbol ]
                            , Table.td [] [ text p.description ]
                            , Table.td [] [ text (String.fromInt p.numberOfShares) ]
                            , Table.td [] [ text (Numeral.format "$0,0.00" p.value) ]
                            , Table.td [] [ text (Numeral.format "$0,0.00" p.price) ]
                            , Table.td [] [ text (String.fromInt p.startShares) ]
                            , Table.td [] [ text (Numeral.format "$0,0.00" p.startAmount) ]
                            , Table.td [] [ text (Numeral.format "0.0%" <| p.startPercent / 100.0) ]
                            , Table.td [] [ text (String.fromInt p.endShares) ]
                            , Table.td [] [ text (Numeral.format "$0,0.00" p.endAmount) ]
                            , Table.td [] [ text (Numeral.format "0.0%" <| p.endPercent / 100.0) ]
                            , Table.td [] [ text (Numeral.format "$0,0.00" p.optimalAmount) ]
                            , Table.td [] [ text (Numeral.format "0.0%" <| p.optimalPercent / 100.0) ]
                            ]
                    ))
                )
            ]
        }

currentPositionsTab : List CP.CurrentPosition -> Tab.Item Msg
currentPositionsTab cp =
    Tab.item
        { id = "tabCurrentPositions"
        , link = Tab.link [] [ text "Current Positions" ]
        , pane = Tab.pane []
            [ Table.simpleTable
                ( Table.simpleThead
                    [ Table.th [Table.cellPrimary] [ text "Account" ]
                    , Table.th [Table.cellPrimary] [ text "Symbol" ]
                    , Table.th [Table.cellPrimary] [ text "Description" ]
                    , Table.th [Table.cellPrimary] [ text "Quantity" ]
                    , Table.th [Table.cellPrimary] [ text "Last Price" ]
                    , Table.th [Table.cellPrimary] [ text "Current Value" ]
                    ]
                , Table.tbody []
                    (cp |> List.map (\p ->
                        Table.tr []
                            [ Table.td [] [ text p.account ]
                            , Table.td [] [ text p.symbol ]
                            , Table.td [] [ text p.description ]
                            , Table.td [] [ text (String.fromInt p.quantity) ]
                            , Table.td [] [ text (Numeral.format "$0,0.00"  p.lastPrice) ]
                            , Table.td [] [ text (Numeral.format "$0,0.00"  p.currentValue) ]
                            ]
                    ))
                )
            ]
        }

missionTab : Float -> Int -> M.Mission -> Tab.Item Msg
missionTab totalWeight idx mission =
    Tab.item
        { id = "tabMission" ++ String.fromInt idx
        , link = Tab.link [] [ text <| mission.name ++ " (" ++ Numeral.format "0%" (Maybe.withDefault mission.defaultWeight mission.weight / totalWeight) ++ ")"]
        , pane = Tab.pane []
            [ Html.div [] <| weightInput idx mission
            , Html.div [] [ text <| "Unweighted cash reserve: " ++ Numeral.format "0.0%" (M.unweightedCashReservePercent mission)]
            , Html.div [] [ text <| "Weighted cash reserve: " ++ Numeral.format "0.0%" (M.weightedCashReservePercent totalWeight mission)]
            , Table.simpleTable
                ( Table.simpleThead
                    [ Table.th [Table.cellPrimary] [ text "Symbol" ]
                    , Table.th [Table.cellPrimary] [ text "Description" ]
                    , Table.th [Table.cellPrimary] [ text "Allocation" ]
                    , Table.th [Table.cellPrimary] [ text "Current Price" ]
                    ]
                , Table.tbody []
                    (M.unweightedPositions mission |> List.map (\p ->
                        Table.tr []
                            [ Table.td [] [ text p.symbol ]
                            , Table.td [] [ text p.description ]
                            , Table.td [] [ text (Numeral.format "0.0%" (p.allocationPercent / 100)) ]
                            , Table.td [] [ text (Numeral.format "$0,0.00" p.currentPrice) ]
                            ]
                    ))
                )
            , Button.button [ Button.onClick (RemoveTargetRequested mission), Button.primary ] [ text "Remove Target" ]
            ]
        }

weightInput : Int -> M.Mission -> List (Html.Html Msg)
weightInput idx mission =
    [ Checkbox.checkbox
        [ Checkbox.id ("useDefaultWeightChk" ++ String.fromInt idx)
        , Checkbox.checked <| U.isNothing mission.weight
        , Checkbox.onCheck (SwitchUseDefaultWeightMsg idx)
        ] "Use default weight"
    , case mission.weight of
        Nothing -> text <| "Fool allocation: " ++ String.fromFloat mission.defaultWeight
        Just _ -> FloatInput.floatInput
            ("defaultWeightInput" ++ String.fromInt idx)
            mission.weightInput
            (UpdateWeightMsg idx)
    ]

addTargetTab : Tab.Item Msg
addTargetTab =
    Tab.item
        { id = "tabAddTarget"
        , link = Tab.link [] [ text "+ Add Target" ]
        , pane = Tab.pane []
            [ p []
                [ text "Cut & paste a Fool portfolio page here to add it as a target:"
                , Textarea.textarea
                    [ Textarea.id "inputAddTarget"
                    , Textarea.rows 4
                    , Textarea.value ""
                    , Textarea.onInput AddTargetInputMsg
                    ]
                ]
            , p []
                [ text "Make sure you are on a portfolio page like the below, then copy the "
                , strong [] [ text "entire" ]
                , text " page, with Ctrl+A, Ctrl+C.  Then click in the above text box and press Ctrl+V."
                ]
            , img
                [ src "fool-portfolio-page.png"
                , style "display" "block"
                , style "width" "70%"
                , style "margin" "auto"
                , style "border" "5px"
                , style "border-style" "outset"
                ] []
            ]
        }

toastyConfig : Toasty.Config msg
toastyConfig =
    Toasty.Defaults.config
        |> Toasty.delay 5000

addToast : Toasty.Defaults.Toast -> (Model, Cmd Msg) -> (Model, Cmd Msg)
addToast toast = Toasty.addToast toastyConfig ToastyMsg toast

addToastError : String -> (Model, Cmd Msg) -> (Model, Cmd Msg)
addToastError error = addToast (Toasty.Defaults.Error "Error" error)

addToastSuccess : String -> (Model, Cmd Msg) -> (Model, Cmd Msg)
addToastSuccess msg = addToast (Toasty.Defaults.Success "Success" msg)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Tab.subscriptions model.tabState TabMsg, Port.recvLocalStore RecvFromJS ]



-- CALCULATE TRANSACTIONS HELPER

recalculate : (Model, Cmd Msg) -> (Model, Cmd Msg)
recalculate (model, msg) =
    let result = T.recalculate model.currentPositions model.cash model.missions model.ignoredSymbols
    in
    ({ model
        | transactions = result.transactions
        , cash = result.cashPosition
        , totalValue = result.totalValue
        , totalValueIgnored = result.totalValueIgnored
        , totalValueForTrades = result.totalValueForTrades
    }, msg)

