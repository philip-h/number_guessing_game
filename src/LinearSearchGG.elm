module LinearSearchGG exposing (..)

import Array exposing (Array)
import Browser exposing (Document)
import Css
import Css.Global
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Random
import Tailwind.Theme as Tw
import Tailwind.Utilities as Tw



-- Model


arraySize : Int
arraySize =
    21


type alias Model =
    { array : Array Int
    , sampleElementIndex : Int
    , currentGuess : Maybe Int
    , guessCount : Int
    , maxGuessesNeeded : Int
    , status : Status
    , winCount : Int
    , loseCount : Int
    }


type Status
    = GameRunning
    | GameWon
    | GameLost
    | GameLoading



-- Update


type Msg
    = ClickedElement Int
    | GotArrayWithSample ( Array Int, Int )
    | PlayAgain


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedElement index ->
            case model.status of
                GameRunning ->
                    ( { model
                        | currentGuess = Just index
                        , guessCount = model.guessCount + 1
                        , status =
                            if index == model.sampleElementIndex then
                                GameWon

                            else if model.guessCount + 1 == model.maxGuessesNeeded then
                                GameLost

                            else
                                GameRunning
                      }
                    , Cmd.none
                    )

                _ ->
                    -- ClickedElement only makes sense when GameRunning
                    ( model, Cmd.none )

        GotArrayWithSample ( array, index ) ->
            ( { model
                | array = array
                , sampleElementIndex = index
                , status = GameRunning
              }
            , Cmd.none
            )

        PlayAgain ->
            ( { model
                | array = Array.empty
                , sampleElementIndex = -1
                , currentGuess = Nothing
                , guessCount = 0
                , status = GameLoading
                , winCount =
                    case model.status of
                        GameWon ->
                            model.winCount + 1

                        _ ->
                            model.winCount
                , loseCount =
                    case model.status of
                        GameLost ->
                            model.loseCount + 1

                        _ ->
                            model.loseCount
              }
            , Random.generate GotArrayWithSample (arrWithSampleGenerator arraySize)
            )



-- View


themeDark : { bg : Tw.Color, surfaceBg : Tw.Color, accent : Tw.Color, text : Tw.Color }
themeDark =
    { bg = Tw.slate_700
    , surfaceBg = Tw.slate_600
    , accent = Tw.purple_400
    , text = Tw.slate_100
    }


view : Model -> Document Msg
view model =
    { title = "Guess it!"
    , body =
        [ toUnstyled <|
            -- Container
            div
                [ css
                    [ Tw.container
                    , Tw.mx_auto
                    , Tw.my_8
                    , Tw.font_mono
                    ]
                ]
                [ Css.Global.global Tw.globalStyles
                , Css.Global.global
                    [ Css.Global.body
                        [ Tw.bg_color themeDark.bg
                        , Tw.text_color themeDark.text
                        ]
                    ]
                , h1
                    [ css [ Tw.text_3xl ] ]
                    [ text "Linear search game" ]
                , div
                    [ css
                        [ Tw.flex
                        , Tw.flex_col
                        , Tw.gap_4
                        , Tw.shadow_lg
                        , Tw.rounded_lg
                        , Tw.p_4
                        , Tw.my_4
                        , Tw.bg_color themeDark.surfaceBg
                        ]
                    ]
                    [ p
                        [ css [ Tw.m_4 ] ]
                      <|
                        case model.currentGuess of
                            -- Use indecies to model binary search
                            Just guess ->
                                let
                                    elementGuessed =
                                        Array.get guess model.array
                                            |> Maybe.withDefault -1
                                in
                                if model.sampleElementIndex /= guess then
                                    [ text "Nope! The number in my head is"
                                    , span [ css [ Tw.text_color themeDark.accent ] ] [ text " NOT " ]
                                    , text (String.fromInt elementGuessed)
                                    ]

                                else
                                    [ text "Wow! You got it :)" ]

                            Nothing ->
                                [ text "I thought of a number and hid it behind one of these boxes. Can you find it?" ]
                    , p [ css [ Tw.m_2 ] ] [ text "[ The numbers behind the boxes are NOT sorted ]" ]
                    , viewArray model
                    , viewStatus model
                    , hr [ css [ Tw.m_4 ] ] []
                    , viewStats model
                    ]
                ]
        ]
    }


viewStats : Model -> Html Msg
viewStats model =
    div [ css [ Tw.flex ] ]
        [ span [ css [ Tw.flex_grow, Tw.text_center, Tw.text_lg ] ] [ text ("Wins : " ++ String.fromInt model.winCount) ]
        , span [ css [ Tw.flex_grow, Tw.text_center, Tw.text_lg ] ] [ text ("Losses : " ++ String.fromInt model.loseCount) ]
        ]


viewArray : Model -> Html Msg
viewArray model =
    div
        [ css
            [ Tw.flex
            , Tw.flex_wrap
            , Tw.items_center
            , Tw.justify_center
            ]
        ]
        (model.array
            |> Array.indexedMap
                (viewArrayElement
                    { indexToGuess = model.sampleElementIndex
                    , currentGuess = Maybe.withDefault -1 model.currentGuess
                    }
                )
            |> Array.toList
        )


viewArrayElement : { indexToGuess : Int, currentGuess : Int } -> Int -> Int -> Html Msg
viewArrayElement { indexToGuess, currentGuess } index element =
    span
        [ css
            [ Tw.flex
            , Tw.border_2
            , Tw.border_color Tw.black
            , Tw.p_4
            , Tw.m_2
            , Tw.rounded
            , Tw.aspect_1
            , Tw.cursor_pointer
            , Css.hover [ Tw.cursor_pointer ]
            , if currentGuess == index then
                if currentGuess == indexToGuess then
                    Tw.bg_color Tw.green_400

                else
                    Tw.bg_color Tw.red_400

              else
                Tw.bg_color themeDark.bg
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
    div [ css [ Tw.text_center ] ] <|
        let
            baseButton =
                styled button
                    [ Tw.px_8, Tw.py_2, Tw.rounded, Tw.m_4 ]

            status =
                case model.status of
                    GameRunning ->
                        [ p [] [ text ("Number of guesses: " ++ String.fromInt model.guessCount ++ " / " ++ String.fromInt model.maxGuessesNeeded) ]
                        , baseButton
                            [ css
                                [ Tw.cursor_not_allowed, Tw.bg_color Tw.gray_400 ]
                            ]
                            [ text "Play again?" ]
                        ]

                    GameWon ->
                        [ p [] [ text ("Congrats! You found the number in " ++ String.fromInt model.guessCount ++ " guesses!") ]
                        , baseButton
                            [ css
                                [ Tw.bg_color themeDark.accent ]
                            , onClick PlayAgain
                            ]
                            [ text "Play again?" ]
                        ]

                    GameLost ->
                        [ p []
                            [ text "Oop! The number in my head was "
                            , span [ css [ Tw.text_color themeDark.accent ] ]
                                [ let
                                    selectedElement =
                                        Array.get model.sampleElementIndex model.array
                                            |> Maybe.withDefault -1
                                            |> String.fromInt
                                  in
                                  text selectedElement
                                ]
                            , text ", which was behind box "
                            , span [ css [ Tw.text_color themeDark.accent ] ]
                                [ text <| String.fromInt model.sampleElementIndex ]
                            , text "! You should have been able to guess that in "
                            , span [ css [ Tw.text_color themeDark.accent ] ]
                                [ text <| String.fromInt model.maxGuessesNeeded ]
                            , text " guesses."
                            ]
                        , baseButton
                            [ css [ Tw.bg_color themeDark.accent ]
                            , onClick PlayAgain
                            ]
                            [ text "Try again?" ]
                        ]

                    GameLoading ->
                        [ p [] [ text "Loading status..." ] ]
        in
        status



-- Main


arrayGenerator : Int -> Random.Generator (Array Int)
arrayGenerator size =
    Random.list size (Random.int 1 99)
        |> Random.map Array.fromList


arrarySampleGenerator : Array Int -> Random.Generator (Maybe Int)
arrarySampleGenerator arr =
    Random.int 0 (Array.length arr - 1)
        |> Random.map (\i -> Array.get i arr)


arrWithSampleGenerator : Int -> Random.Generator ( Array Int, Int )
arrWithSampleGenerator size =
    Random.list size (Random.int 1 99)
        |> Random.andThen
            (\randomList ->
                case randomList of
                    x :: xs ->
                        Random.uniform x xs
                            |> Random.map (\element -> ( randomList, element ))

                    [] ->
                        Random.constant ( [], -1 )
            )
        |> Random.andThen
            (\( randomList, element ) ->
                let
                    indexOf : Int -> Int -> List Int -> Int
                    indexOf index needle haystack =
                        case haystack of
                            [] ->
                                -1

                            x :: xs ->
                                if x == needle then
                                    index

                                else
                                    indexOf (index + 1) needle xs
                in
                Random.constant
                    ( Array.fromList randomList
                    , indexOf 0 element randomList
                    )
            )


init : () -> ( Model, Cmd Msg )
init _ =
    ( { array = Array.empty
      , sampleElementIndex = -1
      , currentGuess = Nothing
      , guessCount = 0
      , maxGuessesNeeded = arraySize
      , status = GameLoading
      , winCount = 0
      , loseCount = 0
      }
    , Random.generate GotArrayWithSample (arrWithSampleGenerator arraySize)
    )


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }

