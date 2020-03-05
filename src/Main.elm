module Main exposing (main)

import Browser exposing (element)
import Card exposing (Card, initCard)
import Html exposing (Html, audio, div, h2, source, text)
import Html.Attributes exposing (class, controls, src, style, type_)
import List exposing (range)
import Random



-- MAIN


main : Program (List String) Model Msg
main =
    Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }



-- MODEL


type alias Model =
    { height : Int
    , width : Int
    , cards : List Card
    , files : List String
    , randomLetters : List Char
    }


init : List String -> ( Model, Cmd Msg )
init files =
    let
        w =
            6

        h =
            7

        initList =
            Random.generate ShuffledCards <| List.map String.fromChar (letters (w * h))
    in
    ( { height = h, width = w, cards = [], files = files, randomLetters = [] }, initList )



-- UPDATE


type Msg
    = ShuffledCards (List String)
    | ShowCard Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShuffledCards files ->
            ( { model | randomLetters = files }, Cmd.none )

        ShowCard index ->
            ( model, Cmd.none )


fillCards : List String -> List Card
fillCards files =
    List.map initCard <| List.indexedMap files


letter : Random.Generator Char
letter =
    Random.map (\n -> Char.fromCode (n + 97)) (Random.int 0 25)


letters : Int -> Random.Generator (List Char)
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
        (cards model.width model.height <| List.map String.fromChar model.randomLetters)


cards : Int -> Int -> List String -> List (Html Msg)
cards w h files =
    List.map card <| List.map2 Tuple.pair (List.range 1 (w * h)) files


card : ( Int, String ) -> Html Msg
card ( n, file ) =
    div [ class "grid-item" ] [ text <| String.fromInt n ++ file ]


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
