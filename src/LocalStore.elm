module LocalStore exposing (setLocalStore, getLocalStore, propRetrieved)

import Json.Encode as E
import Json.Decode as D
import Port

propName : Port.LSSaveable -> E.Value
propName s =
    case s of
        Port.AdjustedSymbols -> E.string "ignoredSymbols"

propSetAdjustedSymbols : List String -> E.Value
propSetAdjustedSymbols s =
    E.object [("prop", propName Port.AdjustedSymbols), ("value", E.list E.string s)]

propGetAdjustedSymbols : E.Value
propGetAdjustedSymbols =
    E.object [("prop", propName Port.AdjustedSymbols)]

propRetrieved : D.Value -> Result String (Maybe Port.LSValueType)
propRetrieved s =
    case D.decodeValue (D.field "prop" D.string) s of
        Err e -> Err <| Debug.toString e
        Ok "ignoredSymbols" ->
            case D.decodeValue (D.field "value" (D.nullable <| D.list D.string)) s of
                Err e -> Err <| Debug.toString e
                Ok (Just list) -> Ok (Just (Port.AdjustedSymbolValue list))
                Ok Nothing -> Ok Nothing
        Ok e -> Err <| "Unknown prop " ++ e

setLocalStore : Port.LSValueType -> Cmd msg
setLocalStore s =
    case s of
        Port.AdjustedSymbolValue list -> Port.setLocalStore <| propSetAdjustedSymbols list

getLocalStore : Port.LSSaveable -> Cmd msg
getLocalStore s =
    case s of
        Port.AdjustedSymbols -> Port.getLocalStore <| propGetAdjustedSymbols
