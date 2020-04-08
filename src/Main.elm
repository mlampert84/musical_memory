port module Main exposing (main)

import Array
import Browser exposing (element)
import Card exposing (Card, Gameboard, fillGameboard, initGameboard)
import Html exposing (Html, audio, div, h2, source, text)
import Html.Attributes exposing (class, controls, src, style, type_)
import Json.Decode as Decode
import Json.Encode as Encode
import List exposing (range)
import Process
import Random
import Random.List exposing (shuffle)
import Task



-- MAIN


port playFile : Encode.Value -> Cmd msg


port playEnded : (() -> msg) -> Sub msg


main : Program (List String) Model Msg
main =
    Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }



-- MODEL


type alias Model =
    { height : Int
    , width : Int
    , gameboard : Gameboard
    , files : List String
    , randomLetters : List String
    , gameState : GameState
    }


type GameState
    = AudioPlaying
    | TeamA
    | TeamB


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
      , gameboard = initGameboard
      , files = files
      , randomLetters = []
      , gameState = TeamA
      }
    , initList
    )


doubleList : List a -> List a
doubleList list =
    list ++ list



-- UPDATE


type Msg
    = DelayCardTurn Int
    | ShuffledCards (List String)
    | ShowCard Int
    | ResolveCards


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DelayCardTurn index ->
            ( model, Process.sleep 2000 |> Task.perform (always (ShowCard index)) )

        ShuffledCards files ->
            ( { model | randomLetters = files, gameboard = fillGameboard files }, Cmd.none )

        ShowCard index ->
            if model.gameState == AudioPlaying then
                ( model, Cmd.none )

            else
                case Card.selectCard model.gameboard index of
                    Card.FirstCard board card ->
                        ( { model | gameboard = board, gameState = AudioPlaying }, playFile (Card.encode card) )

                    Card.SecondCard board card ->
                        ( { model | gameboard = board, gameState = AudioPlaying }, playFile (Card.encode card) )

                    _ ->
                        ( model, Cmd.none )

        ResolveCards ->
            case Card.resolveBoard model.gameboard of
                Card.Match gameboard ->
                    ( { model | gameboard = gameboard, gameState = TeamA }, Cmd.none )

                Card.NoMatch gameboard ->
                    ( { model | gameboard = gameboard, gameState = TeamA }, Cmd.none )

                Card.WaitForNextCard ->
                    ( { model | gameState = TeamA }, Cmd.none )

                _ ->
                    ( model, Cmd.none )


letter : Random.Generator String
letter =
    Random.map String.fromChar <| Random.map (\n -> Char.fromCode (n + 97)) (Random.int 0 25)


letters : Int -> Random.Generator (List String)
letters length =
    Random.list length letter



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    playEnded (always ResolveCards)



-- VIEW


viewGrid : Model -> Html Msg
viewGrid model =
    let
        cssColumns =
            "repeat( " ++ String.fromInt model.width ++ ", 1fr)"
    in
    div [ class "grid-container", style "grid-template-columns" cssColumns ]
        (gridItems model)


gridItems : Model -> List (Html Msg)
gridItems model =
    List.map gridItem <| Array.toList model.gameboard.cards


gridItem : Card -> Html Msg
gridItem card =
    div [ class "grid-item" ] [ Card.view ShowCard card ]


view : Model -> Html Msg
view model =
    div [] [ viewGrid model ]


audioFiles : List String -> List (Html Msg)
audioFiles files =
    let
        toAudioDiv file =
            div [] [ audio [ controls True ] [ source [ src file, type_ "audio/mpeg" ] [] ], h2 [] [ text file ] ]
    in
    List.map toAudioDiv files
