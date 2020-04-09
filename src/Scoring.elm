module Scoring exposing (Scoring, addPoint, changeTurn, init, view)

import Html exposing (Html, div, h2, text)
import Html.Attributes


type alias Scoring =
    { scores : List Score
    , turn : Team
    }


type alias Score =
    ( Team, Int )


type Team
    = A
    | B


init : Scoring
init =
    { scores = [ ( A, 0 ), ( B, 0 ) ]
    , turn = A
    }


teamToText : Team -> String
teamToText t =
    case t of
        A ->
            "A"

        B ->
            "B"


changeTurn : Scoring -> Scoring
changeTurn scoring =
    case scoring.turn of
        A ->
            { scoring | turn = B }

        _ ->
            { scoring | turn = A }


addPoint : Scoring -> Scoring
addPoint scoring =
    { scoring | scores = addPointToTeam scoring.scores scoring.turn }


addPointToTeam : List Score -> Team -> List Score
addPointToTeam scores team =
    let
        point ( t, points ) =
            if team == t then
                ( t, points + 1 )

            else
                ( t, points )
    in
    List.map point scores


scoreToText : Score -> String
scoreToText score =
    teamToText (Tuple.first score) ++ " " ++ String.fromInt (Tuple.second score)


scoresToList : List Score -> String
scoresToList scores =
    String.concat <| List.intersperse " | " << List.map scoreToText <| scores


view : Scoring -> Html msg
view scoring =
    div []
        [ h2 []
            [ text <| "Turn: Team " ++ teamToText scoring.turn ]
        , h2
            []
            [ text <|
                "Score: "
                    ++ scoresToList scoring.scores
            ]
        ]
