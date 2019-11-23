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
    , passwordModel : PasswordModel
    }


type alias PasswordModel =
    { password : String
    , passwordAgain : String
    }


init : Model
init =
    Model "" (PasswordModel "" "")



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
            { model | passwordModel = PasswordModel password model.passwordModel.passwordAgain }

        PasswordAgain passwordAgain ->
            { model | passwordModel = PasswordModel model.passwordModel.password passwordAgain }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewInput "text" "Name" model.name Name
        , viewInput "password" "Password" model.passwordModel.password Password
        , viewInput "password" "Re-enter Password" model.passwordModel.passwordAgain PasswordAgain
        , viewValidation model
        ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []


viewValidation : Model -> Html msg
viewValidation model =
    if isPasswordValid model.passwordModel then
        renderPasswordIsAccepted

    else
        model.passwordModel
            |> passwordIsNotValidReasonList
            |> renderPasswordIsNotValid


isPasswordValid : PasswordModel -> Bool
isPasswordValid passwordModel =
    passwordModel
        |> passwordIsNotValidReasonList
        |> List.isEmpty


renderPasswordIsNotValid : List String -> Html msg
renderPasswordIsNotValid reasons =
    div [ style "color" "black" ]
        [ div [] [ text "Passwords must" ]
        , ul []
            (reasons
                |> List.map (\n -> li [] [ text n ])
            )
        ]


passwordIsNotValidReasonList : PasswordModel -> List String
passwordIsNotValidReasonList passwords =
    let
        checks =
            [ { function = passwordsMustMatch, message = "be equal" }
            , { function = passwordsLengthAreValid, message = "be greater than or equal to 8 characters" }
            , { function = passwordsContainUppercase, message = "contain an uppercase character" }
            , { function = passwordsContainLowercase, message = "contain a lowercase character" }
            , { function = containsNumeric, message = "contains a numeric character" }
            ]
    in
    checks
        |> List.filter (\f -> not (f.function [passwords.password, passwords.passwordAgain] ))
        |> List.map (\n -> n.message)


renderPasswordIsAccepted : Html msg
renderPasswordIsAccepted =
    div [ style "color" "green" ] [ text "OK" ]


passwordsMustMatch : List String -> Bool
passwordsMustMatch passwords =
    List.all (\s -> s == (Maybe.withDefault "" (List.head passwords))) passwords
    

stringsAreEqual : String -> String -> Bool
stringsAreEqual s1 s2 =
    s1 == s2

passwordsLengthAreValid : List String -> Bool
passwordsLengthAreValid strings =
    checkStrings strings (\n -> String.length n >= 8)


checkStrings : List String -> (String -> Bool) -> Bool
checkStrings strings function =
    strings
        |> List.map function
        |> mustAllBeTrue


passwordsContainUppercase : List String -> Bool
passwordsContainUppercase strings =
    strings
    |> List.map (\s -> contains s Char.isUpper)
    |> mustAllBeTrue

mustAllBeTrue : List Bool -> Bool
mustAllBeTrue bools =
    List.all (\b -> b) bools


passwordsContainLowercase : List String -> Bool
passwordsContainLowercase strings =
    strings
    |> List.map (\s -> contains s Char.isLower)
    |> mustAllBeTrue


containsNumeric : List String -> Bool
containsNumeric strings =
    strings
    |> List.map (\s -> contains s Char.isDigit )
    |> mustAllBeTrue


contains : String -> (Char -> Bool) -> Bool
contains input filter =
    not (String.isEmpty (String.filter filter input))
