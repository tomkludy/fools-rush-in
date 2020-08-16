module FloatInput exposing (FloatField, floatInput, updateModel, floatValue, fromFloat)
import Html exposing (Html, text)
import Html.Attributes exposing (style)
import Bootstrap.Form.Input as Input

type FloatField = FloatField (Maybe Float) String

updateModel : String -> (FloatField -> a) -> (Float -> a -> a) -> a
updateModel newString updateInputField onValidInput =
    let
        field = fromString newString
        updated = updateInputField field
    in
    case field of
        FloatField Nothing _ -> updated
        FloatField (Just f) _ -> onValidInput f updated

floatInput : String -> FloatField -> (String -> msg) -> Html msg
floatInput id field msg =
    Input.text
        [ Input.id id
        , Input.small
        , Input.attrs <| floatFieldValidationStyle field
        , Input.value <| stringValue field
        , Input.onInput msg
        ]

stringValue : FloatField -> String
stringValue (FloatField _ s) = s

floatValue : FloatField -> Maybe Float
floatValue (FloatField f _) = f

fromString : String -> FloatField
fromString str =
    let
        f = if str == "" || str == "-"
            then Just 0.0
            else str
                |> String.replace "$" ""
                |> String.replace "%" ""
                |> String.replace "," ""
                |> String.toFloat
    in
    FloatField f str

fromFloat : Float -> FloatField
fromFloat f = FloatField (Just f) (String.fromFloat f)

floatFieldValidationStyle : FloatField -> List (Html.Attribute msg)
floatFieldValidationStyle floatField =
    case floatField of
        FloatField Nothing str ->
            if str == ""
            then []
            else [ Html.Attributes.style "background-color" "red" ]
        _ -> []