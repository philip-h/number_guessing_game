module NumberGuessingGame exposing (..)

import Browser
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Random



-- Model


arraySize : Int
arraySize =
    20


type alias Model =
    { array : List Int
    , randomElement : Int
    , indexOfRandomElement : Int
    , currentGuess : Maybe Int
    , guessCount : Int
    , status : Status
    }


type Status
    = GameRunning
    | GameWon
    | GameLost
    | GameLoading



-- Update


type Msg
    = ClickedElement Int
    | GotArray (List Int)
    | GotRandomElement Int
    | PlayAgain


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedElement index ->
            case model.status of
                GameRunning ->
                    ( { model
                        | currentGuess = Just (Debug.log "The clicked index is " index)
                        , guessCount = model.guessCount + 1
                        , status =
                            if index == Debug.log "The random index is " model.indexOfRandomElement then
                                GameWon

                            else if model.guessCount + 1 == arraySize then
                                GameLost

                            else
                                GameRunning
                      }
                    , Cmd.none
                    )

                _ ->
                    -- ClickedElement only makes sense when GameRunning
                    ( model, Cmd.none )

        GotArray array ->
            case array of
                first :: rest ->
                    Random.uniform first rest
                        |> Random.generate GotRandomElement
                        |> Tuple.pair { model | array = array }

                [] ->
                    ( model, Cmd.none )

        GotRandomElement randomElement ->
            ( { model
                | randomElement = randomElement
                , indexOfRandomElement = Maybe.withDefault -1 <| indexOf randomElement model.array
                , status = GameRunning
              }
            , Cmd.none
            )

        PlayAgain ->
            init ()


indexOfHelper : Int -> comparable -> List comparable -> Maybe Int
indexOfHelper index needle haystack =
    case haystack of
        [] ->
            Nothing

        x :: xs ->
            if x == needle then
                Just index

            else
                indexOfHelper (index + 1) needle xs


indexOf : comparable -> List comparable -> Maybe Int
indexOf needle haystack =
    indexOfHelper 0 needle haystack



-- View


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Linear search game" ]
        , h3 [] [ text ("Looking for " ++ String.fromInt model.randomElement) ]
        , viewArray model
        , viewStatus model
        ]


viewArray : Model -> Html Msg
viewArray model =
    div
        [ css
            [ displayFlex
            , alignItems center
            , justifyContent center
            ]
        ]
        (model.array
            |> List.indexedMap
                (viewArrayElement
                    { indexToGuess = model.indexOfRandomElement
                    , currentGuess = Maybe.withDefault -1 model.currentGuess
                    }
                )
        )


viewArrayElement : { indexToGuess : Int, currentGuess : Int } -> Int -> Int -> Html Msg
viewArrayElement { indexToGuess, currentGuess } index element =
    span
        [ css
            [ displayFlex
            , alignItems end
            , border3 (px 1) solid (rgb 0 0 0)
            , padding (px 15)
            , marginLeft (px 15)
            , hover [ cursor pointer ]
            , backgroundColor
                (if currentGuess == index then
                    if currentGuess == indexToGuess then
                        rgb 0 255 0

                    else
                        rgb 255 0 0

                 else
                    rgb 255 255 255
                )
            ]
        , onClick (ClickedElement index)
        ]
        [ text
            (String.padLeft 2
                '0'
                (String.fromInt
                    (if currentGuess == index then
                        element

                     else
                        index
                    )
                )
            )
        ]


viewStatus : Model -> Html Msg
viewStatus model =
    div [ css [ textAlign center ] ] <|
        let
            message =
                case model.status of
                    GameRunning ->
                        "Number of guesses: " ++ String.fromInt model.guessCount ++ " / " ++ String.fromInt arraySize

                    GameWon ->
                        "Congrats! You found the number in " ++ String.fromInt model.guessCount ++ " guesses!"

                    GameLost ->
                        "Oop! You should have been able to find the number in " ++ String.fromInt arraySize ++ " guesses!"

                    GameLoading ->
                        "Loading..."
        in
        [ p [] [ text message ]
        , div [] [ button [ onClick PlayAgain ] [ text "Play again?" ] ]
        ]



-- Main


listGenerator : Random.Generator (List Int)
listGenerator =
    Random.list arraySize (Random.int 1 99)


init : () -> ( Model, Cmd Msg )
init _ =
    ( { array = []
      , randomElement = -1
      , indexOfRandomElement = -1
      , currentGuess = Nothing
      , guessCount = 0
      , status = GameLoading
      }
    , Random.generate GotArray listGenerator
    )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view >> toUnstyled
        , update = update
        , subscriptions = \_ -> Sub.none
        }
