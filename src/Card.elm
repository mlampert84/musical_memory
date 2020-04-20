module Card exposing (Card, File, State(..), init, view)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class, classList)
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


viewText : Card -> String
viewText card =
    case card.state of
        Closed ->
            String.fromInt (card.index + 1)

        Open ->
            String.fromChar <| Char.fromCode 0x0001D11E

        Removed ->
            ""


view : (Maybe Card -> msg) -> Card -> Html msg
view toggleMsg card =
    div
        [ class "card"
        , classList
            [ ( "card-removed", card.state == Removed )
            , ( "card-closed", card.state == Closed )
            , ( "card-open", card.state == Open )
            ]
        , onClick (toggleMsg (Just card))
        ]
        [ text (viewText card) ]
