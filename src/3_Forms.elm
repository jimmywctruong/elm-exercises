module Main exposing (Model, Msg(..), init, main, update, view, viewInput, viewValidation)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { name : String
    , password : String
    , passwordAgain : String
    }


init : Model
init =
    Model "" "" ""



-- UPDATE


type Msg
    = Name String
    | Password String
    | PasswordAgain String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Name name ->
            { model | name = name }

        Password password ->
            { model | password = password }

        PasswordAgain password ->
            { model | passwordAgain = password }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewInput "text" "Name" model.name Name
        , viewInput "password" "Password" model.password Password
        , viewInput "password" "Re-enter Password" model.passwordAgain PasswordAgain
        , viewValidation model
        ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []


viewValidation : Model -> Html msg
viewValidation model =
    let
        password =
            model.password

        passwordAgain =
            model.passwordAgain
    in
    if passwordLengthIsValid password then
        if password == passwordAgain then
            if passwordIsComplexEnough password then
                renderPasswordIsAccepted

            else
                renderPasswordNotAcceptedWithReason password

        else
            renderPasswordsAreMismatched

    else
        renderPasswordIsTooShort


renderPasswordNotAcceptedWithReason : String -> Html msg
renderPasswordNotAcceptedWithReason password =
    div [ style "color" "gray" ] [ password |> passwordNotAcceptedReason ]


passwordNotAcceptedReason : String -> Html msg
passwordNotAcceptedReason password =
    ul [] (passwordNotAcceptedReasonList password)


passwordNotAcceptedReasonList : String -> List (Html msg)
passwordNotAcceptedReasonList password =
    let
        checks =
            [ { function = containsUppercase, message = "contain an uppercase character" }
            , { function = containsLowercase, message = "contains a lowercase character" }
            , { function = containsNumeric, message = "contains a numeric character" }
            ]
    in
    checks
        |> List.filter (\f -> f.function password)
        |> List.map (\n -> n.message)
        |> assemblePasswordNotAcceptedReasons


assemblePasswordNotAcceptedReasons : List String -> List (Html msg)
assemblePasswordNotAcceptedReasons reasons =
    [ text "stub" ]



-- if String.length reasons > 2 then
-- else if String.length reasons == 2 then
-- reasons
-- |> List.intersperse "and "
-- else
-- li [] [ reasons |> List.head |> withDefault "" ]


renderPasswordIsAccepted : Html msg
renderPasswordIsAccepted =
    div [ style "color" "green" ] [ text "OK" ]


passwordIsComplexEnough : String -> Bool
passwordIsComplexEnough password =
    containsUppercase password && containsLowercase password && containsNumeric password


renderPasswordsAreMismatched : Html msg
renderPasswordsAreMismatched =
    div [ style "color" "red" ] [ text "Passwords do not match!" ]


renderPasswordIsTooShort : Html msg
renderPasswordIsTooShort =
    div [ style "color" "gray" ] [ text "Password is too short!" ]


passwordLengthIsValid : String -> Bool
passwordLengthIsValid password =
    String.length password >= 8


containsUppercase : String -> Bool
containsUppercase input =
    contains input Char.isUpper


containsLowercase : String -> Bool
containsLowercase input =
    contains input Char.isLower


containsNumeric : String -> Bool
containsNumeric input =
    contains input Char.isDigit


contains : String -> (Char -> Bool) -> Bool
contains input filter =
    not (String.isEmpty (String.filter filter input))
