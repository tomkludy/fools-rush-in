import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class)
import File exposing (File)
import File.Select as Select
import Task
import List.Extra as L
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
import Utils as U
import Missions as M
import Transactions as T
import Json.Decode as D
import Json.Encode as E
import Port
import LocalStore as LS

-- MAIN
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
    , totalValueForTrades : Float
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
        , totalValueForTrades = 0.0
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
    | ChangeIgnoreSymbolMsg String Bool
    | SendToJS Port.SendCommand
    | RecvFromJS D.Value

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
                    ( { model
                        | currentPositions = Tuple.first positions
                        , cash = Tuple.second positions
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
            ( { model | cash = CP.setPendingEndCash value model.totalValueForTrades model.cash }, Cmd.none )
                |> recalculate

        UpdateTargetCashMsg ->
            ( { model | cash = CP.commitPendingEndCash model.cash }, Cmd.none )
                |> recalculate

        RevertTargetCashMsg ->
            ( { model | cash = CP.revertPendingEndCash model.cash }, Cmd.none )
                |> recalculate
        
        ChangeIgnoreSymbolMsg symbol ignore ->
            let newIgnored =
                    case ignore of
                        True -> symbol::model.ignoredSymbols
                        False -> List.filter ((/=) symbol) model.ignoredSymbols
            in
            ( { model | ignoredSymbols = newIgnored }
            , LS.setLocalStore <| Port.IgnoredSymbolValue newIgnored
            )
                |> recalculate

        SendToJS cmd  ->
            case cmd of
                Port.SetLocalStore v -> ( model, LS.setLocalStore <| Port.IgnoredSymbolValue model.ignoredSymbols )
                Port.GetLocalStore v -> ( model, LS.getLocalStore Port.IgnoredSymbols )
        
        RecvFromJS incoming ->
            case LS.propRetrieved incoming of
                Err error -> ( model, Cmd.none ) |> addToastError error
                Ok (Just (Port.IgnoredSymbolValue list)) -> ( { model | ignoredSymbols = list }, Cmd.none ) |> recalculate
                Ok Nothing -> ( model, Cmd.none )


-- VIEW

view : Model -> Html Msg
view model =
    case List.length model.currentPositions of
        0 ->
            Grid.container []
                [ CDN.stylesheet
                , Button.button [ Button.onClick CurrentPositionsRequested, Button.primary ] [ text "Load current positions" ]
                , Toasty.view toastyConfig Toasty.Defaults.view ToastyMsg model.toasties
                ]
        _ ->
            Grid.container []
                [ CDN.stylesheet
                , div [] [ text <| "Total value (cash + stock): " ++ Numeral.format "$0,0.00" model.totalValue ]
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
                        [ Table.tr []
                            [ Table.td [] [ text <| Numeral.format "$0,0.00" model.cash.startCash ]
                            , Table.td [] (cashInput model.cash)
                            , Table.td [] [ text <| Numeral.format "0.0%" <| model.cash.desiredEndCash / model.totalValue ]
                            , Table.td [] [ text <| Numeral.format "$0,0.00" model.cash.actualEndCash ]
                            , Table.td [] [ text <| Numeral.format "0.0%" <| model.cash.actualEndCash / model.totalValue ]
                            ]
                        ]
                    )
                , Tab.config TabMsg
                    |> Tab.withAnimation
                    |> Tab.items (List.concat
                        [ [transactionsTab model.ignoredSymbols model.transactions, currentPositionsTab model.currentPositions]
                        , List.indexedMap missionTab model.missions
                        , [addTargetTab]
                        ])
                    |> Tab.view model.tabState
                , Toasty.view toastyConfig Toasty.Defaults.view ToastyMsg model.toasties
                ]

cashInput : CP.CashPosition -> List (Html.Html Msg)
cashInput c =
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
                        [ Input.number
                            [ Input.id "targetCashInput"
                            , Input.small
                            , Input.value (String.fromFloat c.desiredEndCash)
                            , Input.onInput UpdatePendingTargetCashMsg
                            ]
                        ]
                    _ ->
                        [ Input.number
                            [ Input.id "targetCashInput"
                            , Input.small
                            , Input.value (String.fromFloat (Maybe.withDefault 0.0 c.pendingEndCash))
                            , Input.onInput UpdatePendingTargetCashMsg
                            ]
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
            [ Table.simpleTable
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
                    , Table.th [Table.cellPrimary] [ text "Start Percent" ]
                    , Table.th [Table.cellPrimary] [ text "End Shares" ]
                    , Table.th [Table.cellPrimary] [ text "End Amount" ]
                    , Table.th [Table.cellPrimary] [ text "End Percent" ]
                    , Table.th [Table.cellPrimary] [ text "Optimal Amount" ]
                    , Table.th [Table.cellPrimary] [ text "Optimal Percent" ]
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

missionTab : Int -> M.Mission -> Tab.Item Msg
missionTab idx mission =
    Tab.item
        { id = "tabMission" ++ (String.fromInt idx)
        , link = Tab.link [] [ text mission.name ]
        , pane = Tab.pane []
            [ Table.simpleTable
                ( Table.simpleThead
                    [ Table.th [Table.cellPrimary] [ text "Symbol" ]
                    , Table.th [Table.cellPrimary] [ text "Description" ]
                    , Table.th [Table.cellPrimary] [ text "Allocation" ]
                    , Table.th [Table.cellPrimary] [ text "Current Price" ]
                    ]
                , Table.tbody []
                    (mission.missionPositions |> List.map (\p ->
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

addTargetTab : Tab.Item Msg
addTargetTab =
    Tab.item
        { id = "tabAddTarget"
        , link = Tab.link [] [ text "+ Add Target" ]
        , pane = Tab.pane []
            [ text "Cut & paste a Fool portfolio page here to add it as a target"
            , Textarea.textarea
                [ Textarea.id "inputAddTarget"
                , Textarea.rows 4
                , Textarea.value ""
                , Textarea.onInput AddTargetInputMsg
                ]
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
        , totalValueForTrades = result.totalValueForTrades
    }, msg)

