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


type alias PasswordValidator =
    { function : PasswordModel -> Bool
    , message : String
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
    let
        validators =
            [ PasswordValidator passwordsMustMatch "be equal"
            , PasswordValidator passwordLengthIsValid "be greater than or equal to 8 characters"
            , PasswordValidator passwordContainsUppercase "contain an uppercase character"
            , PasswordValidator passwordContainsLowercase "contain a lowercase character"
            , PasswordValidator passwordContainsNumeric "contains a numeric character"
            ]
    in
    if isPasswordValid model.passwordModel validators then
        renderPasswordIsAccepted

    else
        model.passwordModel
            |> renderPasswordValidation validators


renderPasswordValidation : List PasswordValidator -> PasswordModel -> Html msg
renderPasswordValidation validators passwords =
    div [ style "color" "black" ]
        [ text "Passwords must"
        , ul []
            (validators
                |> List.map
                    (\v ->
                        ( v.function passwords, v.message )
                    )
                |> List.map (\t -> li [] [ text (annotatePasswordCriterion (Tuple.first t) (Tuple.second t)) ])
            )
        ]


annotatePasswordCriterion : Bool -> String -> String
annotatePasswordCriterion bool message =
    if bool == True then
        "✔️ " ++ message

    else
        "❌ " ++ message


isPasswordValid : PasswordModel -> List PasswordValidator -> Bool
isPasswordValid passwords validators =
    List.all (\b -> b)
        (validators
            |> List.map (\v -> v.function passwords)
        )


renderPasswordIsAccepted : Html msg
renderPasswordIsAccepted =
    div [ style "color" "green" ] [ text "OK" ]


passwordsMustMatch : PasswordModel -> Bool
passwordsMustMatch passwords =
    passwords.password == passwords.passwordAgain


passwordLengthIsValid : PasswordModel -> Bool
passwordLengthIsValid passwords =
    String.length passwords.password >= 8


passwordContainsUppercase : PasswordModel -> Bool
passwordContainsUppercase passwords =
    contains passwords.password Char.isUpper


passwordContainsLowercase : PasswordModel -> Bool
passwordContainsLowercase passwords =
    contains passwords.password Char.isLower


passwordContainsNumeric : PasswordModel -> Bool
passwordContainsNumeric passwords =
    contains passwords.password Char.isDigit


contains : String -> (Char -> Bool) -> Bool
contains input filter =
    not (String.isEmpty (String.filter filter input))
