module Scoring exposing (Scoring, Team(..), addPoint, changeTurn, init, view)

import Html exposing (Html, div, h2, span, text)
import Html.Attributes exposing (class, classList)


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
    div [ class "flex-container" ] (List.map (teamScore scoring.turn) scoring.scores)


teamScore : Team -> Score -> Html msg
teamScore turn score =
    div [ class "flex-item", classList [ ( "team-turn", turn == Tuple.first score ) ] ]
        [ span [ class "team-label" ] [ text <| "Team " ++ teamToText (Tuple.first score) ]
        , div [ class "score" ] [ text <| String.fromInt (Tuple.second score) ]
        ]
