module Main exposing (main)

import Browser exposing (sandbox)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)
import List exposing (range)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { height : Int
    , width : Int
    }


init : Model
init =
    { height = 6, width = 7 }



-- UPDATE


type Msg
    = Change String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change newContent ->
            model



-- VIEW


view : Model -> Html Msg
view model =
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
