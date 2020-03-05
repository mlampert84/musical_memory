module Card exposing (Card, initCard, openCard, view)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


type alias Card =
    { index : Int
    , file : String
    , state : State
    }


type State
    = Open
    | Closed
    | Removed


initCard : ( Int, String ) -> Card
initCard ( index, file ) =
    Card index file Closed


openCard : Card -> Card
openCard card =
    { card | state = Open }


view : (Int -> msg) -> Card -> Html msg
view toggleMsg card =
    case card.state of
        Removed ->
            text "Removed"

        Closed ->
            div [ class "card-closed", onClick (toggleMsg card.index) ] [ text <| String.fromInt (card.index + 1) ]

        Open ->
            div [ class "card-open" ] [ text card.file ]
