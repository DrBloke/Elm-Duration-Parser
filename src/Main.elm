module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Parser exposing (..)


view : () -> Html msg
view model =
    div [] [ text <| Debug.toString <| run duration "P4Y6M4DT12H30M5S" ]


type alias Duration =
    { years : Float
    , months : Float
    , weeks : Float
    , days : Float
    , hours : Float
    , minutes : Float
    , seconds : Float
    }


duration : Parser Duration
duration =
    succeed Duration
        |. symbol "P"
        |= years
        |= months
        |= weeks
        |= days
        |. symbol "T"
        |= hours
        |= minutes
        |= seconds


years : Parser Float
years =
    oneOf
        [ (succeed identity
            |= float
            |. symbol "Y"
          )
            |> backtrackable
        , succeed 0
        ]


months : Parser Float
months =
    oneOf
        [ (succeed identity
            |= float
            |. symbol "M"
          )
            |> backtrackable
        , succeed 0
        ]


weeks : Parser Float
weeks =
    oneOf
        [ (succeed identity
            |= float
            |. symbol "W"
          )
            |> backtrackable
        , succeed 0
        ]


days : Parser Float
days =
    oneOf
        [ (succeed identity
            |= float
            |. symbol "D"
          )
            |> backtrackable
        , succeed 0
        ]


hours : Parser Float
hours =
    oneOf
        [ (succeed identity
            |= float
            |. symbol "H"
          )
            |> backtrackable
        , succeed 0
        ]


minutes : Parser Float
minutes =
    oneOf
        [ (succeed identity
            |= float
            |. symbol "M"
          )
            |> backtrackable
        , succeed 0
        ]


seconds : Parser Float
seconds =
    oneOf
        [ (succeed identity
            |= float
            |. symbol "S"
          )
            |> backtrackable
        , succeed 0
        ]



--boilerplate


main : Program () () msg
main =
    Browser.sandbox
        { init = ()
        , view = view
        , update = \_ model -> model
        }
