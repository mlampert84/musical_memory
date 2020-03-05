module Main exposing (main)

import Array exposing (Array)
import Browser exposing (element)
import Card exposing (Card, initCard)
import Html exposing (Html, audio, div, h2, source, text)
import Html.Attributes exposing (class, controls, src, style, type_)
import List exposing (range)
import Random
import Random.List exposing (shuffle)



-- MAIN


main : Program (List String) Model Msg
main =
    Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }



-- MODEL


type alias Model =
    { height : Int
    , width : Int
    , cards : Array Card
    , files : List String
    , randomLetters : List String
    }


type alias SelectedCards =
    ( Maybe Card, Maybe Card )


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
    ( { height = h, width = w, cards = Array.empty, files = files, randomLetters = [] }, initList )


doubleList : List a -> List a
doubleList list =
    list ++ list



-- UPDATE


type Msg
    = ShuffledCards (List String)
    | ShowCard Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShuffledCards files ->
            ( { model | randomLetters = files, cards = fillCards files }, Cmd.none )

        ShowCard index ->
            ( { model | cards = openCard index model.cards }, Cmd.none )


openCard : Int -> Array Card -> Array Card
openCard index cards =
    let
        card =
            Array.get index cards
    in
    case card of
        Nothing ->
            cards

        Just c ->
            Array.set index (Card.openCard c) cards


fillCards : List String -> Array Card
fillCards files =
    Array.fromList <| List.map initCard <| List.indexedMap Tuple.pair files


letter : Random.Generator String
letter =
    Random.map String.fromChar <| Random.map (\n -> Char.fromCode (n + 97)) (Random.int 0 25)


letters : Int -> Random.Generator (List String)
letters length =
    Random.list length letter



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



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
    List.map gridItem <| Array.toList model.cards


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
