module CounterGrid exposing (..)


import Html exposing (..)
import Html.Attributes exposing (..)
import Time
import Counter
import List
import Debug
import Random
import Window


-- MODEL

globals = {
    tdSize = "20px"
    , textSize = "1.5em"
    , border = "0px solid gray"
    , w = 40
    , h = 24
    }

type alias Model =
    { counters : Maybe (List Counter.Model)
    , cols : Int
    , rows : Int
    }

init : (Model, Cmd Msg)
init =
    ({ counters = Nothing
     , cols = globals.w
     , rows = globals.h
     }, Random.generate Rnds (periodGen (globals.w*globals.h)))

periodGen : Int -> Random.Generator (List Float)
periodGen n = Random.list n (Random.float 300 30000)

-- UPDATE

type Msg =
    Tick Time.Time
    | CounterMsg Counter.Msg
    | Rnds (List Float)
    | Resize Int Int

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Tick time ->
            case model.counters of
                Just cs ->
                    ({ model
                    | counters = Just <| List.map (Counter.update <| Counter.tickMsg time) cs
                    }, Cmd.none)
                Nothing -> (model, Cmd.none)
        CounterMsg counterMsg ->
            (model, Cmd.none)
        Rnds periods ->
            ({ model
             | counters = Just <| List.map (\p -> Counter.init globals.textSize p 0) periods
            }, Cmd.none)
        Resize y x ->
            (model, Cmd.none)





-- VIEW

viewTable : Model -> Html Msg
viewTable model =
    case model.counters of
    Nothing -> div [] []
    Just cs ->
        let
            w = model.cols
            h = ((List.length cs) // model.cols) + 1
            n = List.length cs
            row r = tr [] <| List.map (counterAt r) <| List.range 0 (w-1)
            counterAt r i = td [tdStyle] <|
                case get cs (r*w + i) of
                    Nothing -> []
                    Just cell -> [Html.map CounterMsg <| Counter.view cell]
        in
            div []
                [ table [tableStyle]
                    <| List.map row <| List.range 0 (h-1)
                ]

tdStyle = style
    [ ("width", globals.tdSize)
    , ("height", globals.tdSize)
    ]

tableStyle = style
    [ ("background", "black")
    , ("border", globals.border)
    , ("margin", "0 auto")
    ]

view : Model -> Html Msg
view model =
    div [ style
            [ ("background", "black")
            , ("position", "absolute")
            , ("width", "100%")
            , ("height", "100%")
            ]
        ]
        [ viewTable model
        ]

get : List a -> Int -> Maybe a
-- get: Returns the element at index i.
get list i = List.drop i list |> List.head


-- APP

subscriptions : Model -> Sub Msg
subscriptions model = Sub.batch
    [ Time.every Time.millisecond Tick
    , Window.resizes (\{height, width} -> Resize height width)
    ]
