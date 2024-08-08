module DiscoBinary exposing (..)

import Array exposing (Array)
import Browser exposing (Document)
import Css exposing (decimal)
import Css.Global
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (checked, css, disabled, for, id, type_, value)
import Html.Styled.Events exposing (onClick, onInput)
import Tailwind.Theme as Tw
import Tailwind.Utilities as Tw



-- Model


type alias Model =
    { bits : Array Bit
    , showingLabels : Bool
    , showingBits : Bool
    , showingButtons : Bool
    , editingEntries : Bool
    }


type Bit
    = Zero
    | One


bitToString : Bit -> String
bitToString bit =
    case bit of
        Zero ->
            "0"

        One ->
            "1"


bitToInt : Bit -> Int
bitToInt bit =
    case bit of
        Zero ->
            0

        One ->
            1


bitsToString : Array Bit -> String
bitsToString bits =
    bits
        |> Array.map bitToString
        |> Array.foldl (++) ""


bitsToInt : Array Bit -> Int
bitsToInt bits =
    bits
        |> Array.indexedMap
            (\index bit ->
                bitToInt bit * 2 ^ index
            )
        |> Array.foldl (+) 0


bitsToAscii : Array Bit -> Char
bitsToAscii bits =
    Char.fromCode <| bitsToInt bits


toggleBit : Bit -> Bit
toggleBit bit =
    case bit of
        Zero ->
            One

        One ->
            Zero


incrementBits : Array Bit -> Array Bit
incrementBits bits =
    let
        decimal =
            bitsToInt bits
    in
    if decimal < 255 then
        intTo8Bits (decimal + 1)

    else
        bits


decrementBits : Array Bit -> Array Bit
decrementBits bits =
    let
        decimal =
            bitsToInt bits
    in
    if decimal > 0 then
        intTo8Bits (decimal - 1)

    else
        bits


bitArrayTo8Bits : Array Bit -> Array Bit
bitArrayTo8Bits bits =
    case compare (Array.length bits) 8 of
        GT ->
            Array.slice 0 8 bits

        LT ->
            Array.append bits (Array.repeat (8 - Array.length bits) Zero)

        EQ ->
            bits


intTo8Bits : Int -> Array Bit
intTo8Bits decimal =
    let
        intToBits : Int -> List Bit
        intToBits decimal_ =
            if decimal_ <= 0 then
                []

            else if decimal_ >= 255 then
                List.repeat 8 One

            else
                case modBy 2 decimal_ of
                    0 ->
                        Zero :: intToBits (decimal_ // 2)

                    _ ->
                        One :: intToBits (decimal_ // 2)

        arrayOfBits : Array Bit
        arrayOfBits =
            Array.fromList (intToBits decimal)
    in
    bitArrayTo8Bits arrayOfBits


stringTo8Bits : String -> Maybe (Array Bit)
stringTo8Bits binary =
    let
        arrayOfBits : Maybe (Array Bit)
        arrayOfBits =
            case String.uncons binary of
                Nothing ->
                    Just Array.empty

                Just ( char, rest ) ->
                    if char == '0' then
                        Maybe.map (Array.push Zero) (stringTo8Bits rest)

                    else if char == '1' then
                        Maybe.map (Array.push One) (stringTo8Bits rest)

                    else
                        Nothing
    in
    arrayOfBits



-- Update


type Msg
    = ClickedBit Int
    | ToggleShowingLabels
    | ToggleShowingBits
    | ToggleShowingButtons
    | ToggleEditingEntries
    | ShowAll
    | HideAll
    | FillBits
    | Increment
    | Decrement
    | ClearBits
    | BinaryEntryChanged String
    | DecimalEntryChanged String
    | AsciiEntryChanged String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "msg" msg of
        ClickedBit index ->
            let
                currBit =
                    Array.get index model.bits

                newBits =
                    case currBit of
                        Just bit ->
                            Array.set index (toggleBit bit) model.bits

                        Nothing ->
                            model.bits
            in
            ( { model | bits = newBits }, Cmd.none )

        ToggleShowingLabels ->
            ( { model | showingLabels = not model.showingLabels }, Cmd.none )

        ToggleShowingBits ->
            ( { model | showingBits = not model.showingBits }, Cmd.none )

        ToggleShowingButtons ->
            ( { model | showingButtons = not model.showingButtons }, Cmd.none )

        ToggleEditingEntries ->
            ( { model | editingEntries = not model.editingEntries }, Cmd.none )

        ShowAll ->
            ( { model
                | showingLabels = True
                , showingBits = True
                , showingButtons = True
                , editingEntries = True
              }
            , Cmd.none
            )

        HideAll ->
            ( { model
                | showingLabels = False
                , showingBits = False
                , showingButtons = False
                , editingEntries = False
              }
            , Cmd.none
            )

        FillBits ->
            ( { model | bits = intTo8Bits 255 }, Cmd.none )

        Increment ->
            ( { model | bits = incrementBits model.bits }, Cmd.none )

        Decrement ->
            ( { model | bits = decrementBits model.bits }, Cmd.none )

        ClearBits ->
            ( { model | bits = intTo8Bits 0 }, Cmd.none )

        BinaryEntryChanged text ->
            ( { model
                | bits =
                    case stringTo8Bits (String.padLeft 8 '0' (String.right 8 text)) of
                        Just bits ->
                            bits

                        Nothing ->
                            model.bits
              }
            , Cmd.none
            )

        DecimalEntryChanged text ->
            ( { model
                | bits =
                    case String.toInt text of
                        Just decimal ->
                            intTo8Bits decimal

                        Nothing ->
                            -- Because this msg comes from an input type number, the only way for this to be Nothing is blank
                            intTo8Bits 0
              }
            , Cmd.none
            )

        AsciiEntryChanged text ->
            ( { model
                | bits =
                    case String.uncons text of
                        Just ( _, rest ) ->
                            case String.uncons rest of
                                Just ( char, "" ) ->
                                    intTo8Bits (Char.toCode char)

                                _ ->
                                    intTo8Bits 0

                        Nothing ->
                            intTo8Bits 0
              }
            , Cmd.none
            )



-- View


themeDark : { bg : Tw.Color, surfaceBg : Tw.Color, accent : Tw.Color, text : Tw.Color }
themeDark =
    { bg = Tw.zinc_700
    , surfaceBg = Tw.zinc_600
    , accent = Tw.purple_400
    , text = Tw.zinc_100
    }


view : Model -> Document Msg
view model =
    { title = "Discover Binary!"
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
                    [ span [ css [ Tw.text_color themeDark.accent ] ] [ text "Discover " ]
                    , text "Binary Numbers"
                    ]
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
                    [ view8Bits model
                    , hr [ css [ Tw.m_4 ] ] []
                    , viewModButtons model.showingButtons
                    ]
                , div [ css [ Tw.flex, Tw.justify_around ] ]
                    [ viewSettings model
                    , viewManualEntry model
                    ]
                ]
        ]
    }


viewSettings : Model -> Html Msg
viewSettings model =
    div [ css [ Tw.mt_16, Tw.p_4 ] ]
        [ h2 [ css [ Tw.mb_4, Tw.text_xl ] ] [ text "Settings" ]
        , div [ css [ Tw.mb_4 ] ]
            [ button
                [ css [ Tw.mr_2, Css.hover [ Tw.text_color themeDark.accent ] ]
                , onClick ShowAll
                ]
                [ text "Show all" ]
            , text " / "
            , button
                [ css [ Tw.ml_2, Css.hover [ Tw.text_color themeDark.accent ] ]
                , onClick HideAll
                ]
                [ text "Hide all" ]
            ]
        , table []
            [ tr []
                [ td [] [ text "Show labels" ]
                , td [ css [ Tw.px_4 ] ]
                    [ input
                        [ type_ "checkbox"
                        , checked model.showingLabels
                        , onClick ToggleShowingLabels
                        ]
                        []
                    ]
                ]
            , tr []
                [ td [] [ text "Show Bits" ]
                , td [ css [ Tw.px_4 ] ]
                    [ input
                        [ type_ "checkbox"
                        , checked model.showingBits
                        , onClick ToggleShowingBits
                        ]
                        []
                    ]
                ]
            , tr []
                [ td [] [ text "Show Buttons" ]
                , td [ css [ Tw.px_4 ] ]
                    [ input
                        [ type_ "checkbox"
                        , checked model.showingButtons
                        , onClick ToggleShowingButtons
                        ]
                        []
                    ]
                ]
            , tr []
                [ td [] [ text "Edit Entries" ]
                , td [ css [ Tw.px_4 ] ]
                    [ input
                        [ type_ "checkbox"
                        , checked model.editingEntries
                        , onClick ToggleEditingEntries
                        ]
                        []
                    ]
                ]
            ]
        ]


coolInput :
    { type__ : String
    , id_ : String
    , label_ : String
    , value_ : String
    , editing : Bool
    }
    -> (String -> msg)
    -> Html msg
coolInput { type__, id_, label_, value_, editing } msg =
    div
        [ css
            [ Tw.flex
            , Tw.justify_between
            , Tw.items_center
            , Tw.mb_4
            ]
        ]
        [ label [ for id_ ] [ text label_ ]
        , input
            [ css
                [ Tw.ml_4
                , Tw.bg_color themeDark.surfaceBg
                , Tw.rounded
                ]
            , id id_
            , type_ type__
            , value value_
            , onInput msg
            , disabled (not editing)
            ]
            []
        ]


viewManualEntry : Model -> Html Msg
viewManualEntry model =
    div [ css [ Tw.mt_16, Tw.p_4 ] ]
        [ h2 [ css [ Tw.mb_4, Tw.text_xl ] ]
            [ if model.editingEntries then
                text "Manual Entry"

              else
                text "Binary Representations"
            ]
        , coolInput
            { type__ = "text"
            , id_ = "binary-input"
            , label_ = "Binary"
            , value_ = bitsToString model.bits
            , editing = model.editingEntries
            }
            BinaryEntryChanged
        , coolInput
            { type__ = "number"
            , id_ = "decimal-input"
            , label_ = "Decimal"
            , value_ = String.fromInt <| bitsToInt model.bits
            , editing = model.editingEntries
            }
            DecimalEntryChanged
        , coolInput
            { type__ = "text" 
            , id_ = "ascii-input"
            , label_ = "ASCII"
            , value_ = String.fromChar <| bitsToAscii model.bits
            , editing = model.editingEntries
            }
            AsciiEntryChanged
        ]


viewModButtons : Bool -> Html Msg
viewModButtons showingButtons =
    div [ css [ Tw.flex, Tw.justify_around, Tw.items_center ] ] <|
        let
            styledButton =
                styled button
                    [ Tw.p_2
                    , Tw.rounded_lg
                    , Tw.border_2
                    , Tw.border_color Tw.black
                    , Tw.bg_color themeDark.bg
                    , if showingButtons then
                        Tw.visible

                      else
                        Tw.invisible
                    , Css.hover [ Tw.bg_color themeDark.surfaceBg ]
                    ]
        in
        [ styledButton [ onClick FillBits ] [ text "Fill bits" ]
        , styledButton [ onClick Increment ] [ text "+1" ]
        , styledButton [ onClick Decrement ] [ text "-1" ]
        , styledButton [ onClick ClearBits ] [ text "Clear Bits" ]
        ]


view8Bits : Model -> Html Msg
view8Bits model =
    div
        [ css
            [ Tw.flex
            , Tw.flex_row_reverse
            , Tw.flex_wrap
            , Tw.items_center
            , Tw.justify_center
            ]
        ]
        (model.bits
            |> Array.indexedMap (viewBit model.showingBits model.showingLabels)
            |> Array.toList
        )


viewBit : Bool -> Bool -> Int -> Bit -> Html Msg
viewBit showingBits showingLabels index bit =
    div
        [ css
            [ Tw.flex
            , Tw.flex_col
            , Tw.items_center
            , Tw.justify_between
            ]
        ]
        [ span
            [ css
                (if not showingLabels then
                    [ Tw.invisible ]

                 else
                    []
                )
            ]
            [ text (String.fromInt (2 ^ index)) ]
        , button
            [ css
                [ Tw.flex
                , Tw.items_center
                , Tw.justify_center
                , Tw.border_2
                , Tw.border_color Tw.black
                , Tw.h_14
                , Tw.m_2
                , Tw.rounded
                , Tw.aspect_1
                , Tw.cursor_pointer
                , Css.hover [ Tw.cursor_pointer ]
                , bitBackgroundColor bit
                ]
            , onClick (ClickedBit index)
            ]
            [ if showingBits then
                text (String.fromInt <| bitToInt bit)

              else
                text ""
            ]
        ]


bitBackgroundColor : Bit -> Css.Style
bitBackgroundColor bit =
    case bit of
        Zero ->
            Css.batch
                [ Tw.bg_color themeDark.bg
                , Css.hover
                    [ Tw.bg_color themeDark.surfaceBg
                    ]
                ]

        One ->
            Css.batch
                [ Tw.bg_color Tw.green_700
                , Css.hover [ Tw.bg_color Tw.green_600 ]
                ]



-- Main


init : () -> ( Model, Cmd Msg )
init _ =
    ( { bits = Array.repeat 8 Zero
      , showingLabels = False
      , showingBits = False
      , showingButtons = False
      , editingEntries = False
      }
    , Cmd.none
    )


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
