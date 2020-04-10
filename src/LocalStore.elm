module LocalStore exposing (setLocalStore, getLocalStore, propRetrieved)

import Json.Encode as E
import Json.Decode as D
import Port

propName : Port.LSSaveable -> E.Value
propName s =
    case s of
        Port.IgnoredSymbols -> E.string "ignoredSymbols"

propSetIgnoredSymbols : List String -> E.Value
propSetIgnoredSymbols s =
    E.object [("prop", propName Port.IgnoredSymbols), ("value", E.list E.string s)]

propGetIgnoredSymbols : E.Value
propGetIgnoredSymbols =
    E.object [("prop", propName Port.IgnoredSymbols)]

propRetrieved : D.Value -> Result String (Maybe Port.LSValueType)
propRetrieved s =
    case D.decodeValue (D.field "prop" D.string) s of
        Err e -> Err <| Debug.toString e
        Ok "ignoredSymbols" ->
            case D.decodeValue (D.field "value" (D.nullable <| D.list D.string)) s of
                Err e -> Err <| Debug.toString e
                Ok (Just list) -> Ok (Just (Port.IgnoredSymbolValue list))
                Ok Nothing -> Ok Nothing
        Ok e -> Err <| "Unknown prop " ++ e

setLocalStore : Port.LSValueType -> Cmd msg
setLocalStore s =
    case s of
        Port.IgnoredSymbolValue list -> Port.setLocalStore <| propSetIgnoredSymbols list

getLocalStore : Port.LSSaveable -> Cmd msg
getLocalStore s =
    case s of
        Port.IgnoredSymbols -> Port.getLocalStore <| propGetIgnoredSymbols
