module Main exposing (..)

import Html exposing (Html, program)
import CounterGrid as CounterGrid exposing (init, update, view, Model, Msg, subscriptions)

n = 18

main : Program Never Model Msg
main = program
    { init = init (List.take n rnds) 0
    , view = view
    , update = update
    , subscriptions = subscriptions}

rnds = [6506, 651, 1125, 2228, 4854, 2221, 524, 3690, 2165, 6649, 7216, 5865, 113, 1179, 5156, 677, 4623, 772, 7658, 4631, 3548, 5222, 5692, 1582, 2067, 1474, 1184, 2622, 2397, 4178, 7143, 1058, 6251, 615, 3007, 1639, 2316, 7520, 2164, 4463, 1109, 5505, 5145, 3565, 5336, 6137, 4383, 1659, 6994, 5385, 3350, 4380, 4661, 2381, 4654, 5978, 5894, 4450, 367, 1093, 5619, 7516, 3326, 7425, 1741, 1731, 3072, 5422, 7793, 7139, 5643, 2660, 3151, 5240, 2578, 4478, 2256, 3403, 7856, 2287, 4523, 7586, 7627, 818, 4197, 4401, 4465, 6540, 2863, 7545, 5673, 2099, 5407, 6367, 1863, 827, 3375, 226, 1014, 5438]