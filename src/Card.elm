module Card exposing (Card, State(..), init, view)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick)


type alias Card =
    { index : Int
    , file : File
    , state : State
    }


type alias File =
    String


type State
    = Open
    | Closed
    | Removed


init : ( Int, String ) -> Card
init ( index, file ) =
    Card index file Closed


view : (Maybe Card -> msg) -> Card -> Html msg
view toggleMsg card =
    case card.state of
        Removed ->
            div [ class "card", class "card-removed" ] []

        Closed ->
            div [ class "card", class "card-closed", id card.file, onClick (toggleMsg (Just card)) ] [ text <| String.fromInt (card.index + 1) ]

        Open ->
            div [ class "card", class "card-open" ] [ text card.file ]
