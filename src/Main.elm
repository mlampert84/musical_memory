module Main exposing (main)

import Browser exposing (element)
import Html exposing (Html, audio, div, h2, source, text)
import Html.Attributes exposing (class, controls, src, style, type_)
import List exposing (range)



-- MAIN


main =
    Browser.element { init = init, subscriptions = subscriptions, update = update, view = view }



-- MODEL


type alias Model =
    { height : Int
    , width : Int
    , files : List String
    }


init : List String -> ( Model, Cmd Msg )
init files =
    ( { height = 6, width = 7, files = files }, Cmd.none )



-- UPDATE


type Msg
    = Change String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Change newContent ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


viewGrid : Model -> Html Msg
viewGrid model =
    let
        cssColumns =
            "repeat( " ++ String.fromInt model.width ++ ", 1fr)"
    in
    div [ class "grid-container", style "grid-template-columns" cssColumns ]
        (cells model.width model.height)


cells : Int -> Int -> List (Html Msg)
cells w h =
    List.concat <| List.map (makeRow w) <| List.range 1 h


makeRow : Int -> Int -> List (Html Msg)
makeRow width height =
    let
        toDiv x =
            div [ class "grid-item" ] [ text (String.fromInt (x + width * (height - 1))) ]
    in
    List.map toDiv <| List.range 1 width


view : Model -> Html Msg
view model =
    div [] (audioFiles model.files)


audioFiles : List String -> List (Html Msg)
audioFiles files =
    let
        toAudioDiv file =
            div [] [ audio [ controls True ] [ source [ src file, type_ "audio/mpeg" ] [] ], h2 [] [ text file ] ]
    in
    List.map toAudioDiv files
