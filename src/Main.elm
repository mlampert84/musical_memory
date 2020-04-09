port module Main exposing (main)

import Array
import Browser exposing (element)
import Card exposing (Card)
import Gameboard
import Html exposing (Html, div)
import Html.Attributes exposing (class, style)
import Random
import Random.List exposing (shuffle)
import Scoring exposing (Scoring)



-- MAIN


port playFile : String -> Cmd msg


port playEnded : (() -> msg) -> Sub msg


main : Program (List String) Model Msg
main =
    Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }



-- MODEL


type alias Model =
    { height : Int
    , width : Int
    , gameboard : Gameboard.Model
    , files : List String
    , audioPlaying : Bool
    , scoring : Scoring
    }


init : List String -> ( Model, Cmd Msg )
init files =
    let
        w =
            3

        h =
            4

        initList =
            Random.generate ShuffledCards <| shuffle <| doubleList <| List.take ((w * h) // 2) files
    in
    ( { height = h
      , width = w
      , gameboard = Array.empty
      , files = files
      , audioPlaying = False
      , scoring = Scoring.init
      }
    , initList
    )


doubleList : List a -> List a
doubleList list =
    list ++ list



-- UPDATE


type Msg
    = ShuffledCards (List String)
    | UpdateCards (Maybe Card)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShuffledCards files ->
            ( { model | gameboard = Gameboard.fill files }, Cmd.none )

        UpdateCards maybeCard ->
            case Gameboard.update maybeCard model.gameboard of
                ( board, Gameboard.CardTurned card ) ->
                    if model.audioPlaying then
                        ( model, Cmd.none )

                    else
                        ( { model | gameboard = board, audioPlaying = True }, playFile card.file )

                ( board, Gameboard.Match ) ->
                    ( { model | gameboard = board, audioPlaying = False, scoring = Scoring.addPoint model.scoring }, Cmd.none )

                ( board, Gameboard.NoMatch ) ->
                    ( { model | gameboard = board, audioPlaying = False, scoring = Scoring.changeTurn model.scoring }, Cmd.none )

                ( _, Gameboard.WaitForNextCard ) ->
                    ( { model | audioPlaying = False }, Cmd.none )

                _ ->
                    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    playEnded (always (UpdateCards Nothing))



-- VIEW


viewGrid : Model -> Html Msg
viewGrid model =
    let
        cssColumns =
            "repeat( " ++ String.fromInt model.width ++ ", 1fr)"
    in
    div [ class "grid-container", style "grid-template-columns" cssColumns ]
        (Gameboard.gridItems UpdateCards model.gameboard)


view : Model -> Html Msg
view model =
    div []
        [ Scoring.view model.scoring
        , div [] [ viewGrid model ]
        ]
