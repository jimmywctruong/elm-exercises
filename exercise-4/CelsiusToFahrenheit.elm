module CelsiusToFahrenheit exposing (..)

import Browser
import Html exposing (Attribute, Html, div, input, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { celsius : String
    , fahrenheit : String
    , inches : String
    }


init : Model
init =
    { celsius = ""
    , fahrenheit = ""
    , inches = ""
    }



-- UPDATE


type Msg
    = Celsius String
    | Fahrenheit String
    | Inches String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Celsius celsius ->
            { model | celsius = celsius }

        Fahrenheit fahrenheit ->
            { model | fahrenheit = fahrenheit }

        Inches inches ->
            { model | inches = inches }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ case String.toFloat model.celsius of
            Just celsius ->
                viewCelsiusToFahrenheit model.celsius (celsius |> convertCelsiusToFahrenheit |> String.fromFloat)

            Nothing ->
                viewCelsiusToFahrenheitInvalid model.celsius "???"
        , Html.br [] []
        , case String.toFloat model.fahrenheit of
            Just fahrenheit ->
                viewFahrenheitToCelsius model.fahrenheit (fahrenheit |> convertFahrenheitToCelsius |> String.fromFloat)

            Nothing ->
                viewFahrenheitToCelsiusInvalid model.fahrenheit "???"
        , Html.br [] []
        , case String.toFloat model.inches of
            Just inches ->
                viewInchesToCentimetres model.inches (inches |> convertInchesToCentimetres |> String.fromFloat)

            Nothing ->
                viewInchesToCentimetresInvalid model.inches "???"
        ]


convertInchesToCentimetres : Float -> Float
convertInchesToCentimetres inches =
    inches * 2.54


convertCelsiusToFahrenheit : Float -> Float
convertCelsiusToFahrenheit celsius =
    celsius * 1.8 + 32


convertFahrenheitToCelsius : Float -> Float
convertFahrenheitToCelsius fahrenheit =
    (fahrenheit - 32) / 1.8


viewInchesToCentimetres : String -> String -> Html Msg
viewInchesToCentimetres previous current =
    viewGenerator "in" "cm" previous current "blue" Inches


viewInchesToCentimetresInvalid : String -> String -> Html Msg
viewInchesToCentimetresInvalid previous current =
    viewGenerator "in" "cm" previous current "red" Inches


viewFahrenheitToCelsius : String -> String -> Html Msg
viewFahrenheitToCelsius previous current =
    viewGenerator "°F" "°C" previous current "blue" Fahrenheit


viewFahrenheitToCelsiusInvalid : String -> String -> Html Msg
viewFahrenheitToCelsiusInvalid previous current =
    viewGenerator "°F" "°C" previous current "red" Fahrenheit


viewCelsiusToFahrenheit : String -> String -> Html Msg
viewCelsiusToFahrenheit previous current =
    viewGenerator "°C" "°F" previous current "blue" Celsius


viewCelsiusToFahrenheitInvalid : String -> String -> Html Msg
viewCelsiusToFahrenheitInvalid previous current =
    viewGenerator "°C" "°F" previous current "red" Celsius


viewGenerator : String -> String -> String -> String -> String -> (String -> Msg) -> Html Msg
viewGenerator fromUnitLabel toUnitLabel previousValue currentValue currentColour converter =
    span []
        [ input [ value previousValue, onInput converter, style "width" "40px", style "border-color" currentColour ] []
        , text (" " ++ fromUnitLabel ++ " = ")
        , span [ style "color" currentColour ] [ text currentValue ]
        , text (" " ++ toUnitLabel)
        ]
