port module Port exposing (getLocalStore, setLocalStore, recvLocalStore, LSSaveable(..), LSValueType(..))

import Json.Encode as E
import Json.Decode as D

-- type SendCommand
--     = SetLocalStore LSValueType
--     | GetLocalStore LSSaveable

type LSSaveable
    = IgnoredSymbols

type LSValueType
    = IgnoredSymbolValue (List String)


port setLocalStore : E.Value -> Cmd msg
port getLocalStore : E.Value -> Cmd msg
port recvLocalStore : (D.Value -> msg) -> Sub msg