module Counter exposing (Model, init, Msg, tickMsg, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Time
import Debug
import String exposing (concat)


-- MODEL

type alias Model =
    { num : Int
    , limit : Int
    , color : String
    , period : Time.Time
    , start : Time.Time
    , fontSize : String
    }


init : String -> Float -> Model
init fontSize period =
    { num = -1
    , limit = 10
    , color = "rgb(50,100,255)"
    , period = period
    , fontSize = fontSize
    , start = 0
    }

-- UPDATE

type Msg = Tick Time.Time

tickMsg : Time.Time -> Msg
tickMsg t = Tick t

update : Msg -> Model -> Model
-- Update checks if the last tick was over one period ago. If so, increment the state.
update msg model =
    case msg of
        Tick time ->
            if (time - model.start) > model.period
            then { model
                 | num = (model.num + 1) % model.limit
                 , start = time
                 }
            else model

-- VIEW

view : Model -> Html Msg
-- Each counter is just some coloured text and a number.
view model =
    div []
        [ div [style <| numberStyle model] [ text <| textFor model.num ]
        ]

textFor : Int -> String
textFor n =
    if n > 0
    then toString n
    else "0"

numberStyle : Model -> List ( String, String )
numberStyle model =
    [ ("font-family", "monospace")
    , ("text-align", "center")
    , ("font-size", model.fontSize)
    , ("font-weight", "bold")
    ] ++
    -- Text glow, using CSS3. Tested on Chrome, not sure about other browsers.
    if model.num > 0
    then [ ("color", model.color)
         , ("text-shadow", concat ["-1px 1px 20px ", model.color, ", 1px -1px 20px ", model.color])
         ]
    else [ ("color", "black")]