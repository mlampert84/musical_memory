module Card exposing (Card, Gameboard, Msg(..), SelectedCards, State(..), changeCards, encode, fillCards, fillGameboard, initCard, initGameboard, resolveBoard, selectCard, view)

import Array exposing (Array)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Json.Encode as Encode


type alias Gameboard =
    { selected : SelectedCards
    , cards : Array Card
    }


initGameboard : Gameboard
initGameboard =
    Gameboard ( Nothing, Nothing ) Array.empty


fillGameboard : List String -> Gameboard
fillGameboard files =
    Gameboard ( Nothing, Nothing ) (fillCards files)


fillCards : List String -> Array Card
fillCards files =
    Array.fromList <| List.map initCard <| List.indexedMap Tuple.pair files


type alias Card =
    { index : Int
    , file : File
    , state : State
    }


encode : Card -> Encode.Value
encode card =
    Encode.object
        [ ( "file", Encode.string card.file )
        , ( "index", Encode.int card.index )
        ]


type alias File =
    String


type alias SelectedCards =
    ( Maybe Card, Maybe Card )


type State
    = Open
    | Closed
    | Removed


initCard : ( Int, String ) -> Card
initCard ( index, file ) =
    Card index file Closed


type Msg
    = None
    | FirstCard Gameboard Card
    | SecondCard Gameboard Card
    | WaitForNextCard
    | Match Gameboard
    | NoMatch Gameboard


selectCard : Gameboard -> Int -> Msg
selectCard board index =
    let
        newCards =
            changeCards index Open board.cards

        maybeTurnedCard =
            Array.get index newCards
    in
    case maybeTurnedCard of
        Nothing ->
            None

        Just turnedCard ->
            case board.selected of
                ( Nothing, Nothing ) ->
                    FirstCard (Gameboard ( Just turnedCard, Nothing ) newCards) turnedCard

                ( Just card, Nothing ) ->
                    if card.index == index then
                        None

                    else
                        SecondCard (Gameboard ( Just card, Just turnedCard ) newCards) turnedCard

                _ ->
                    None


resolveBoard : Gameboard -> Msg
resolveBoard board =
    case board.selected of
        ( Just _, Nothing ) ->
            WaitForNextCard

        ( Just card1, Just card2 ) ->
            if card1.file == card2.file then
                Match
                    (Gameboard ( Nothing, Nothing )
                        (changeCards card2.index Removed <|
                            changeCards card1.index Removed board.cards
                        )
                    )

            else
                NoMatch
                    (Gameboard ( Nothing, Nothing )
                        (changeCards card2.index Closed <|
                            changeCards card1.index Closed board.cards
                        )
                    )

        _ ->
            None


changeCards : Int -> State -> Array Card -> Array Card
changeCards index desiredState cards =
    let
        card =
            Array.get index cards
    in
    case card of
        Nothing ->
            cards

        Just c ->
            Array.set index { c | state = desiredState } cards


view : (Int -> msg) -> Card -> Html msg
view toggleMsg card =
    case card.state of
        Removed ->
            text "Removed"

        Closed ->
            div [ class "card-closed", id card.file, onClick (toggleMsg card.index) ] [ text <| String.fromInt (card.index + 1) ]

        Open ->
            div [ class "card-open" ] [ text card.file ]
