module Main exposing (..)

import Html exposing (Html, program)
import CounterGrid as CounterGrid exposing (init, update, view, Model, Msg, subscriptions)

main : Program Never Model Msg
main = program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions}
