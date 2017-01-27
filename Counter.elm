module Counter exposing (Model, init, Msg, tickMsg, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Time
import Debug
import String exposing (concat)

import SevenSeg exposing (sevenSeg)

-- MODEL

type alias Model =
    { num : Int
    , limit : Int
    , color : String
    , period : Time.Time
    , start : Time.Time
    , fontSize : Float
    }


init : Float -> Float -> Model
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
        [ div [] [ sevenSeg (model.fontSize*0.7) model.fontSize model.color model.num ]
        ]
