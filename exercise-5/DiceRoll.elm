module DiceRoll exposing (..)

-- Press a button to generate a random number between 1 and 6.
--
-- Read how it works:
--   https://guide.elm-lang.org/effects/random.html
--

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { dieFace : Face
    }


type Face
    = One
    | Two
    | Three
    | Four
    | Five
    | Six


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model One
    , Cmd.none
    )



-- UPDATE


type Msg
    = Roll
    | NewFace Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            ( model
            , Random.generate NewFace (Random.int 1 6)
            )

        NewFace newFace ->
            ( diceModelFromNumber newFace
            , Cmd.none
            )


diceModelFromNumber : Int -> Model
diceModelFromNumber n =
    Model
        (case n of
            1 ->
                One

            2 ->
                Two

            3 ->
                Three

            4 ->
                Four

            5 ->
                Five

            6 ->
                Six

            _ ->
                One
        )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewDieFace model.dieFace
        , button [ onClick Roll ] [ text "Roll" ]
        ]


viewDieFace : Face -> Html Msg
viewDieFace dieFace =
    -- text (dieFace |> dieFaceNumber |> String.fromInt)
    img [ src (dieFaceFileName dieFace), height 50, width 50 ] []


dieFaceFileName : Face -> String
dieFaceFileName dieFace =
    "die" ++ String.fromInt (dieFaceNumber dieFace) ++ ".svg"


dieFaceNumber : Face -> Int
dieFaceNumber dieFace =
    case dieFace of
        One ->
            1

        Two ->
            2

        Three ->
            3

        Four ->
            4

        Five ->
            5

        Six ->
            6
