module SevenSeg exposing (sevenSeg)

import Html exposing (..)
import Html.Attributes exposing (..)


-- SEVEN SEGMENT DISPLAY

{-
     A
     -
  B | | C
     - D
  E | | F
     -
     G
-}

type Orientation = Horizontal | Vertical
type State = On | Off
type alias Style =
    { width : Float
    , height: Float
    , colour : String
    }

sevenSeg : Float -> Float -> String -> Int -> Html msg
sevenSeg width height colour number =
    let
        segStyle =
            { width = width
            , height = height
            , colour = colour
            }
        
        horizontalStyle =
            [ ("display", "flex")
            , ("justify-content", "center")
            ]

        verticalStyle =
            [ ("display", "flex")
            , ("justify-content", "space-between")
            ]
    in
        Html.div
            [ style <| sevenSegStyle segStyle ]
            [ Html.div [ style horizontalStyle ] [ sevenSegA segStyle number ]
            , Html.div [ style verticalStyle ]   [ sevenSegB segStyle number, sevenSegC segStyle number ]
            , Html.div [ style horizontalStyle ] [ sevenSegD segStyle number ]
            , Html.div [ style verticalStyle ]   [ sevenSegE segStyle number, sevenSegF segStyle number ]
            , Html.div [ style horizontalStyle ] [ sevenSegG segStyle number ]
            ]

sevenSegStyle : Style -> List (String, String)
sevenSegStyle style =
    [ ("display", "inline-block")
    , ("height", pixels <| style.height)
    , ("width", pixels <| style.width)
    ]


-- SEGMENTS

sevenSegA style number =
    segment Horizontal style
    <| case number of
        1 -> Off
        2 -> On
        3 -> On
        4 -> Off
        5 -> On
        6 -> On
        7 -> On
        8 -> On
        9 -> On
        _ -> Off
 
sevenSegB style number =
    segment Vertical style
    <| case number of
        1 -> Off
        2 -> Off
        3 -> Off
        4 -> On
        5 -> On
        6 -> On
        7 -> Off
        8 -> On
        9 -> On
        _ -> Off

sevenSegC style number =
    segment Vertical style
    <| case number of
        1 -> On
        2 -> On
        3 -> On
        4 -> On
        5 -> Off
        6 -> Off
        7 -> On
        8 -> On
        9 -> On
        _ -> Off

sevenSegD style number =
    segment Horizontal style
    <| case number of
        1 -> Off
        2 -> On
        3 -> On
        4 -> On
        5 -> On
        6 -> On
        7 -> Off
        8 -> On
        9 -> On
        _ -> Off

sevenSegE style number =
    segment Vertical style
    <| case number of
        1 -> Off
        2 -> On
        3 -> Off
        4 -> Off
        5 -> Off
        6 -> On
        7 -> Off
        8 -> On
        9 -> Off
        _ -> Off

sevenSegF style number =
    segment Vertical style
    <| case number of
        1 -> On
        2 -> Off
        3 -> On
        4 -> On
        5 -> On
        6 -> On
        7 -> On
        8 -> On
        9 -> On
        _ -> Off

sevenSegG style number =
    segment Horizontal style
    <| case number of
        1 -> Off
        2 -> On
        3 -> On
        4 -> Off
        5 -> On
        6 -> On
        7 -> Off
        8 -> On
        9 -> On
        _ -> Off

segment : Orientation -> Style -> State -> Html msg
segment orientation segStyle state =
    case orientation of
        Horizontal ->
            Html.div
                [ style <| segmentStyle orientation segStyle state ]
                []
        Vertical ->
            Html.div
                [ style <| segmentStyle orientation segStyle state ]
                []

segmentStyle : Orientation -> Style -> State -> List ( String, String )
segmentStyle orientation style state =
    let
        segmentWidth = toFloat <| ceiling <| style.height/16
    in
        [ ("display",  "inline-block")
        , case orientation of
            Horizontal -> ("height", pixels <| segmentWidth)
            Vertical   -> ("height", pixels <| (style.height-segmentWidth*3)/2 - 2)
        , case orientation of
            Horizontal -> ("width", pixels <| style.width - (4*segmentWidth))
            Vertical   -> ("width", pixels <| segmentWidth)
        , case state of
            On  -> ("background-color", style.colour)
            Off -> ("background-color", "#080808")
        , case orientation of
            Horizontal -> ("margin", pixels <| 0)
            Vertical   -> ("margin", pixels <| 1)
        , case state of
            On  -> ("box-shadow", "0px 0px 4px 1.5px " ++ style.colour)
            Off -> ("box-shadow", "none")
        ]


-- OTHER

pixels : Float -> String
pixels number =
    toString number ++ "px"
