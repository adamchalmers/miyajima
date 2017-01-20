module CounterGrid exposing (..)


import Html exposing (..)
import Html.Attributes exposing (..)
import Time
import Counter
import List
import Debug

-- MODEL

type alias Model =
    { counters : List Counter.Model
    , cols : Int
    }

init : List Float -> Time.Time -> (Model, Cmd Msg)
init periods start =
    let
        m = { counters = List.map (\p -> Counter.init p start) periods
            , cols = 4
            }
    in
        (m, Cmd.none)

-- UPDATE

type Msg = Tick Time.Time | CounterMsg Counter.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Tick time ->
            ({ model
            | counters = List.map (Counter.update <| Counter.tickMsg time) model.counters
            }, Cmd.none)
        CounterMsg counterMsg ->
            (model, Cmd.none)


-- VIEW

viewTable : Model -> Html Msg
viewTable model =
    let
        w = model.cols
        h = ((List.length model.counters) // model.cols) + 1
        n = List.length model.counters
        row r = tr [] <| List.map (counterAt r) <| List.range 0 (w-1)
        counterAt r i = case get model.counters (r*w + i) of
            Nothing -> td [] []
            Just cell -> td [] [Html.map CounterMsg <| Counter.view cell]
    in
        div []
            [ table [style [("background", "black")]]
                <| List.map row <| List.range 0 (h-1)
            ]

view : Model -> Html Msg
view model =
    div []
        [ viewTable model
        ]

get : List a -> Int -> Maybe a
-- get: Returns the element at index i.
get list i = List.drop i list |> List.head


-- APP

subscriptions : Model -> Sub Msg
subscriptions model = Sub.batch
    [ Time.every Time.second Tick
    ]
