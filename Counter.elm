module Counter exposing (..)

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
    , color = blue
    , period = Time.inSeconds period
    , start = Time.inSeconds startTime
    }


-- UPDATE

type Msg = Tick Time.Time

tickMsg : Time.Time -> Msg
tickMsg t = Tick t

update : Msg -> Model -> Model
update msg model =
    case msg of
        Tick time ->
            let
                delta = Debug.log "delta" <| Time.inSeconds <| time - model.start
            in
                if delta > (Debug.log "period" <| model.period)
                then { model
                     | num = (model.num + 1) % model.limit
                     , start = time
                     }
                else model



-- VIEW

view : Model -> Html Msg
view model =
    let
        rgb col =
            let
                rgba = Color.toRgb col
                ts = toString
            in concat ["rgb(", ts rgba.red, ",", ts rgba.green, ",", ts rgba.blue, ")"]
        countStyle model =
            [ ("font-size", "20px")
            , ("font-family", "monospace")
            , ("display", "inline-block")
            , ("width", "50px")
            , ("color", rgb model.color)
            , ("text-align", "center")
            ]
    in
        div []
            [ div [ style <| countStyle model ] [ text (toString <| model.num) ]
            ]




