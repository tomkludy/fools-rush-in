module IntInput exposing (IntField, intInput, updateModel, intValue, fromInt)
import Html exposing (Html, text)
import Html.Attributes exposing (style)
import Bootstrap.Form.Input as Input

type IntField = IntField (Maybe Int) String

updateModel : String -> (IntField -> a) -> (Int -> a -> a) -> a
updateModel newString updateInputField onValidInput =
    let
        field = fromString newString
        updated = updateInputField field
    in
    case field of
        IntField Nothing _ -> updated
        IntField (Just f) _ -> onValidInput f updated

intInput : String -> IntField -> (String -> msg) -> Html msg
intInput id field msg =
    Input.text
        [ Input.id id
        , Input.small
        , Input.attrs <| intFieldValidationStyle field
        , Input.value <| stringValue field
        , Input.onInput msg
        ]

stringValue : IntField -> String
stringValue (IntField _ s) = s

intValue : IntField -> Maybe Int
intValue (IntField f _) = f

fromString : String -> IntField
fromString str =
    let
        f = if str == "" || str == "-"
            then Just 0
            else str
                |> String.replace "$" ""
                |> String.replace "%" ""
                |> String.replace "," ""
                |> String.toInt
    in
    IntField f str

fromInt : Int -> IntField
fromInt f = IntField (Just f) (String.fromInt f)

intFieldValidationStyle : IntField -> List (Html.Attribute msg)
intFieldValidationStyle intField =
    case intField of
        IntField Nothing str ->
            if str == ""
            then []
            else [ Html.Attributes.style "background-color" "red" ]
        _ -> []