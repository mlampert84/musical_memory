module Card exposing (Card)


type alias Card =
    { index : Int
    , file : String
    , state : State
    }


type State
    = Open
    | Close
