module Card exposing (Card, Gameboard, Msg(..), SelectedCards, State(..), changeCards, fillCards, fillGameboard, initCard, initGameboard, resolveBoard, selectCard, view)

import Array exposing (Array)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


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
    , file : String
    , state : State
    }


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
    | FirstCard Gameboard
    | SecondCard Gameboard
    | Match Gameboard
    | NoMatch Gameboard


selectCard : Gameboard -> Int -> Msg
selectCard board index =
    let
        newCards =
            changeCards index Open board.cards

        turnedCard =
            Array.get index newCards
    in
    case board.selected of
        ( Nothing, Nothing ) ->
            FirstCard (Gameboard ( turnedCard, Nothing ) newCards)

        ( Just card, Nothing ) ->
            if card.index == index then
                None

            else
                SecondCard (Gameboard ( Just card, turnedCard ) newCards)

        _ ->
            None


resolveBoard : Gameboard -> Msg
resolveBoard board =
    case board.selected of
        ( Just card1, Just card2 ) ->
            if card1.file == card2.file then
                Match (closeBoard board Removed)

            else
                NoMatch (closeBoard board Closed)

        _ ->
            None


closeBoard : Gameboard -> State -> Gameboard
closeBoard board desiredState =
    case board.selected of
        ( Just card1, Just card2 ) ->
            Gameboard ( Nothing, Nothing )
                (changeCards card2.index desiredState <| changeCards card1.index desiredState board.cards)

        _ ->
            board


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
            div [ class "card-closed", onClick (toggleMsg card.index) ] [ text <| String.fromInt (card.index + 1) ]

        Open ->
            div [ class "card-open" ] [ text card.file ]
