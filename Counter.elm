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
    , fontSize : String
    }


init : String -> Float -> Time.Time -> Model
init fontSize period startTime =
    { num = -1
    , limit = 10
    , color = Color.rgb 50 50 255
    , period = period
    , fontSize = fontSize
    , start = Time.inMilliseconds startTime
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
        [ div [style <| numberStyle model]
            [ text <| textFor model.num
            ]
        ]

textFor : Int -> String
textFor n =
    if n > 0
    then toString n
    else "0"

rgb : Color -> String
rgb col = concat
    [ "rgb("
    , toString (Color.toRgb col).red, ","
    , toString (Color.toRgb col).green, ","
    , toString (Color.toRgb col).blue, ")"
    ]

numberStyle : Model -> List ( String, String )
numberStyle model =
    [ ("font-family", "monospace")
    , ("text-align", "center")
    , ("font-size", model.fontSize)
    , ("font-weight", "bold")
    ] ++
    -- Text glow, using CSS3. Tested on Chrome, not sure about other browsers.
    if model.num > 0
    then [ ("color", rgb model.color)
         , ("text-shadow", "-1px 1px 20px " ++ rgb model.color ++ ", 1px -1px 20px " ++ rgb model.color)
         ]
    else [ ("color", "black")]