module CounterGrid exposing (..)


import Html exposing (..)
import Html.Attributes exposing (..)
import Time
import Counter
import List
import Debug
import Random
import Window
import Task
import Array as A exposing (Array, get)
import Html.Lazy exposing (lazy)


-- MODEL

globals =
    let
        tdPx = 20
    in
        { tdPx = tdPx
        , tdSize = (toString tdPx) ++ "px"
        , textSize = "1.5em"
        , border = "0px solid gray"
        , w = 40
        , h = 24
        }

type alias Model =
    { counters : Maybe (Array Counter.Model)
    , cols : Maybe Int
    , rows : Maybe Int
    }

init : (Model, Cmd Msg)
init =
    ({ counters = Nothing
     , cols = Nothing
     , rows = Nothing
    }, Task.perform Resize Window.size)

periodGen : Int -> Random.Generator (List Float)
periodGen n = Random.list n (Random.float 300 30000)

-- UPDATE

type Msg =
    -- Pass the tick to each counter so they can update if their period is over.
    Tick Time.Time
    -- Not used in practice
    | CounterMsg Counter.Msg
    -- Used only once at start of app, when the random period lengths are generated.
    | Rnds (List Float)
    -- Used to determine how many cols/rows we need to fill the screen.
    | Resize Window.Size

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of


        Tick time ->
            case model.counters of
                Just cs ->
                    ({ model
                    | counters = Just <| A.map (Counter.update <| Counter.tickMsg time) cs
                    }, Cmd.none)
                -- Ignore ticks if the counters haven't been initialized yet.
                Nothing -> (model, Cmd.none)

        CounterMsg counterMsg ->
            (model, Cmd.none)

        -- Once we receive a list of periods, we can initialize the counters with them.
        Rnds periods ->
            ({ model
             | counters = Just <| A.map (\p -> Counter.init globals.textSize p) (A.fromList periods)
            }, Cmd.none)

        -- Once we have a window size we know how many counters we'll need, so we can generate period lengths.
        Resize windowSize ->
            let
                cols = windowSize.width // globals.tdPx
                rows = windowSize.height // globals.tdPx
                m = { model
                    | cols = Just cols
                    , rows = Just rows
                    }
            in
                (m, Random.generate Rnds (periodGen <| cols*rows))


-- VIEW

viewTable : Model -> Html Msg
viewTable model =
    case (model.counters, model.cols, model.rows) of

        (Just cs, Just cols, Just rows) ->
            let
                n = A.length cs
                w = cols
                h = (n // cols) + 1
                row r = tr [] <| A.toList <| A.map (lazy cellFor) <| A.slice (r*h) (r*h + w) cs
                cellFor counter = td [tdStyle] [Html.map CounterMsg <| lazy Counter.view counter]
            in
                div []
                    [ table [tableStyle] (List.map row <| List.range 0 (h-1))
                    ]

        (_, _, _) -> div [] []

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


-- APP

subscriptions : Model -> Sub Msg
subscriptions model = Sub.batch
    [ Time.every Time.millisecond Tick
    , Window.resizes Resize
    ]
