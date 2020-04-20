port module Main exposing (main)

import Array
import Browser exposing (element)
import Card exposing (Card)
import Gameboard
import Html exposing (Html, div)
import Html.Attributes exposing (class, classList, style)
import Random
import Random.List exposing (shuffle)
import Scoring exposing (Scoring)



-- MAIN


port playFile : String -> Cmd msg


port pauseFile : String -> Cmd msg


port resumeFile : String -> Cmd msg


port stopFile : () -> Cmd msg


port playEnded : (() -> msg) -> Sub msg


main : Program (List FlagFile) Model Msg
main =
    Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }



-- MODEL


type alias FlagFile =
    { file : Card.File
    }


type alias Audio =
    { paused : Bool
    , file : Card.File
    }


type alias Model =
    { height : Int
    , width : Int
    , gameboard : Gameboard.Model
    , files : List FlagFile
    , audio : Maybe Audio
    , scoring : Scoring
    }


init : List FlagFile -> ( Model, Cmd Msg )
init files =
    let
        w =
            3

        h =
            2

        initList =
            Random.generate ShuffledCards <| shuffle <| doubleList <| List.map .file <| List.take ((w * h) // 2) files
    in
    ( { height = h
      , width = w
      , gameboard = Array.empty
      , files = files
      , audio = Nothing
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


pauseAudio : Audio -> Audio
pauseAudio audio =
    { audio | paused = True }


resumeAudio : Audio -> Audio
resumeAudio audio =
    { audio | paused = False }


startAudio : Card.File -> Audio
startAudio file =
    { paused = False, file = file }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShuffledCards files ->
            ( { model | gameboard = Gameboard.fill files }, Cmd.none )

        UpdateCards maybeCard ->
            case Gameboard.update maybeCard model.gameboard of
                ( board, Gameboard.CardTurned card ) ->
                    ( { model | gameboard = board, audio = Just { paused = False, file = card.file } }, playFile card.file )

                ( _, Gameboard.SameSecondCard _ ) ->
                    ( model, stopFile () )

                ( board, Gameboard.Match ) ->
                    ( { model | gameboard = board, audio = Nothing, scoring = Scoring.addPoint model.scoring }, Cmd.none )

                ( board, Gameboard.NoMatch ) ->
                    ( { model | gameboard = board, audio = Nothing, scoring = Scoring.changeTurn model.scoring }, Cmd.none )

                ( _, Gameboard.WaitForNextCard ) ->
                    ( { model | audio = Nothing }, Cmd.none )

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
    div
        [ classList
            [ ( "body-div", True )
            , ( "team-a", model.scoring.turn == Scoring.A )
            , ( "team-b", model.scoring.turn == Scoring.B )
            ]
        ]
        [ Scoring.view model.scoring
        , div [] [ viewGrid model ]
        ]
