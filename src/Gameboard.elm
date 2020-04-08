module Gameboard exposing (Model, Msg(..), fill, gridItems, update)

import Array exposing (Array)
import Card exposing (Card)
import Html exposing (Html, div)
import Html.Attributes exposing (class)


type alias Model =
    Array Card


fill : List String -> Model
fill =
    Array.fromList << List.map Card.init << List.indexedMap Tuple.pair


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


listOpenCards : Model -> List Card
listOpenCards cards =
    let
        isOpen card =
            card.state == Card.Open
    in
    Array.toList << Array.filter isOpen <| cards


selectCard : Card -> Model -> ( Model, Msg )
selectCard card model =
    let
        newCards =
            changeCard card Card.Open model
    in
    case listOpenCards model of
        [] ->
            ( newCards, CardTurned card )

        [ card1 ] ->
            if card1.index == card.index then
                ( model, None )

            else
                ( newCards, CardTurned card )

        _ ->
            ( model, None )


resolveBoard : Model -> ( Model, Msg )
resolveBoard model =
    case listOpenCards model of
        [ _ ] ->
            ( model, WaitForNextCard )

        [ card1, card2 ] ->
            if card1.file == card2.file then
                ( changeCards ( card1, card2 ) Card.Removed model, Match )

            else
                ( changeCards ( card1, card2 ) Card.Closed model, NoMatch )

        _ ->
            ( model, None )


changeCards : ( Card, Card ) -> Card.State -> Model -> Model
changeCards ( card1, card2 ) state cards =
    changeCard card2 state << changeCard card1 state <| cards


changeCard : Card -> Card.State -> Model -> Model
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
    List.map (gridItem selectMsg) <| Array.toList model


gridItem : (Maybe Card -> msg) -> Card -> Html msg
gridItem selectMsg card =
    div [ class "grid-item" ] [ Card.view selectMsg card ]
