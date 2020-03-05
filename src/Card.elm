module Card exposing (Card, initCard)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)


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


view : msg -> Card -> Html msg
view toggleMsg card =
    case card.state of
        Removed ->
            text ""

        Closed ->
            div [ class "card-closed" ] [ text <| String.fromInt card.index ]

        Open ->
            text ""
