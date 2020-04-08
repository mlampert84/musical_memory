module Gameboard exposing (Model, Msg(..), fill, gridItems, init, update)

import Array exposing (Array)
import Card exposing (Card)
import Html exposing (Html, div)
import Html.Attributes exposing (class)


type alias SelectedCards =
    ( Maybe Card, Maybe Card )


type alias Model =
    { selected : SelectedCards
    , cards : Array Card
    }


init : Model
init =
    { selected = ( Nothing, Nothing ), cards = Array.empty }


fill : List String -> Model
fill files =
    let
        fillCards =
            Array.fromList << List.map Card.init << List.indexedMap Tuple.pair
    in
    { selected = ( Nothing, Nothing ), cards = fillCards files }


type Msg
    = None
    | CardTurned Card
    | WaitForNextCard
    | Match
    | NoMatch


update : Maybe Card -> Model -> ( Model, Msg )
update newCard model =
    case newCard of
        Just card ->
            selectCard card model

        Nothing ->
            resolveBoard model


selectCard : Card -> Model -> ( Model, Msg )
selectCard card model =
    let
        newCards =
            changeCard card Card.Open model.cards
    in
    case model.selected of
        ( Nothing, Nothing ) ->
            ( { selected = ( Array.get card.index newCards, Nothing ), cards = newCards }, CardTurned card )

        ( Just card1, Nothing ) ->
            if card1.index == card.index then
                ( model, None )

            else
                ( { selected = ( Just card1, Array.get card.index newCards ), cards = newCards }, CardTurned card )

        _ ->
            ( model, None )


resolveBoard : Model -> ( Model, Msg )
resolveBoard model =
    case model.selected of
        ( Just _, Nothing ) ->
            ( model, WaitForNextCard )

        ( Just card1, Just card2 ) ->
            if card1.file == card2.file then
                ( Model ( Nothing, Nothing ) <| changeCards ( card1, card2 ) Card.Removed model.cards, Match )

            else
                ( Model ( Nothing, Nothing ) <| changeCards ( card1, card2 ) Card.Closed model.cards, NoMatch )

        _ ->
            ( model, None )


changeCards : ( Card, Card ) -> Card.State -> Array Card -> Array Card
changeCards ( card1, card2 ) state cards =
    changeCard card2 state << changeCard card1 state <| cards


changeCard : Card -> Card.State -> Array Card -> Array Card
changeCard card desiredState cards =
    let
        selectedCard =
            Array.get card.index cards
    in
    case selectedCard of
        Nothing ->
            cards

        Just c ->
            Array.set card.index { c | state = desiredState } cards


gridItems : (Maybe Card -> msg) -> Model -> List (Html msg)
gridItems selectMsg model =
    List.map (gridItem selectMsg) <| Array.toList model.cards


gridItem : (Maybe Card -> msg) -> Card -> Html msg
gridItem selectMsg card =
    div [ class "grid-item" ] [ Card.view selectMsg card ]
