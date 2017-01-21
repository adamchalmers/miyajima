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
import Array exposing (Array, get)

-- MODEL

globals = {
    tdPx = 15
    , textSize = "1.5em"
    , border = "0px solid gray"
    }

type alias Model =
    -- counters is instantiated at runtime, once random period lengths are generated.
    { counters : Maybe (Array Counter.Model)
    -- cols and rows are instantiated at runtime, once window size is determined.
    , cols : Maybe Int
    , rows : Maybe Int
    }

init : (Model, Cmd Msg)
-- First cols/rows are calculated (with a Window.Size task), then counters are (with a Random task).
init =
    ({ counters = Nothing
     , cols = Nothing
     , rows = Nothing
    --  }, Random.generate Rnds (periodGen n))
     }, Task.perform Resize Window.size)

periodGen : Int -> Random.Generator (List Float)
periodGen n = Random.list n (Random.float 300 30000)

-- UPDATE

type Msg =
    Tick Time.Time
    | CounterMsg Counter.Msg
    | Rnds (List Float)
    | Resize Window.Size

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of

        Tick time ->
            case model.counters of
                Just cs ->
                    ({ model
                    | counters = Just <| Array.indexedMap (\i c -> Counter.update (Counter.tickMsg time (neighbours i model)) c) cs
                    }, Cmd.none)
                Nothing -> (model, Cmd.none)

        CounterMsg counterMsg ->
            (model, Cmd.none)

        Rnds periods ->
            ({ model
            | counters = Just <| Array.fromList <|
                List.map2 (\p id -> Counter.init globals.textSize p 0 id)
                periods
                (List.range 0 (-1 + List.length periods))
            }, Cmd.none)

        Resize size ->
            let
                cols = size.width // globals.tdPx
                rows = size.width // globals.tdPx
                m = { model
                    | cols = Just cols
                    , rows = Just rows
                    }
            in
                (m, Random.generate Rnds (periodGen <| cols*rows))

neighbours : Int -> Model -> List Counter.State
neighbours target model =
    case (model.counters, model.cols, model.rows) of
    (Nothing, _, _) -> Debug.crash "tried to find neighbours of uninitialized grid."
    (_, Nothing, _) -> Debug.crash "tried to find neighbours of uninitialized grid."
    (_, _, Nothing) -> Debug.crash "tried to find neighbours of uninitialized grid."
    (Just cs, Just cols, Just rows) ->
        let
            n = Array.length cs
            y = target // cols
            x = target % cols
            undo x y = (y * cols) + x
            adjacentIDs = Debug.log (toString target) <| Array.fromList [ undo x (y+1), undo x (y-1), undo (x+1) y, undo (x-1) y ]
            adjacents = List.filterMap identity <| Array.toList <| Array.map (\i -> Array.get i cs) adjacentIDs
        in
            List.map .num adjacents
-- VIEW

viewTable : Model -> Html Msg
viewTable model =
    case (model.counters, model.cols, model.rows) of
    (Nothing, _, _) -> div [] []
    (_, Nothing, _) -> div [] []
    (_, _, Nothing) -> div [] []
    (Just cs, Just cols, Just rows) ->
        let
            h = ((Array.length cs) // cols) + 1
            n = Array.length cs
            row r = tr [] <| List.map (counterAt r) <| List.range 0 (cols-1)
            counterAt r i = td [tdStyle] <|
                case Array.get (r*cols + i) cs of
                    Nothing -> []
                    Just cell -> [Html.map CounterMsg <| Counter.view cell]
        in
            div []
                [ table [tableStyle]
                    <| List.map row <| List.range 0 (h-1)
                ]

tdStyle = style
    [ ("width", (toString globals.tdPx) ++ "px")
    , ("height", (toString globals.tdPx) ++ "px")
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
