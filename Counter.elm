module Counter exposing (Model, init, Msg, tickMsg, update, view)

import Html exposing (..)
import Color exposing (Color, red, green, blue, black)
import Html.Attributes exposing (..)
import Time
import Debug
import String exposing (concat)


-- MODEL

type alias Model =
    { num : Int
    , limit : Int
    , color : Color
    , period : Time.Time
    , start : Time.Time
    }


init : Float -> Time.Time -> Model
init period startTime =
    { num = 0
    , limit = 9
    , color = red
    , period = Debug.log "p" <| period
    , start = Debug.log "s" <| Time.inMilliseconds startTime
    }

-- UPDATE

type Msg = Tick Time.Time

tickMsg : Time.Time -> Msg
tickMsg t = Tick t

update : Msg -> Model -> Model
update msg model =
    case msg of
        Tick time ->
            if (Time.inMilliseconds <| time - model.start) > (model.period)
            then { model
                  | num = (model.num + 1) % model.limit
                  , start = time
                  }
            else model

-- VIEW

view : Model -> Html Msg
view model =
    div []
        [ div [style <| countStyle model]
            [ text (if model.num > 0 then (toString <| model.num) else "")
            ]
        ]

rgb : Color -> String
rgb col =
    let
        rgba = Color.toRgb col
        ts = toString
    in concat ["rgb(", ts rgba.red, ",", ts rgba.green, ",", ts rgba.blue, ")"]

countStyle : Model -> List ( String, String )
countStyle model =
    [ ("font-family", "monospace")
    , ("color", rgb model.color)
    , ("text-align", "center")
    , ("font-size", "4em")
    ]
