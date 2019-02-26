module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Parser exposing ((|.), (|=), Parser, backtrackable, float, int, oneOf, run, succeed, symbol)


view : () -> Html msg
view model =
    div []
        [ div [] [ text <| Debug.toString <| run duration "P4Y6M4DT12H30M5S" ]
        , div [] [ text <| Debug.toString <| run duration "P5D" ]
        , div [] [ text <| Debug.toString <| run duration "P5DT5M" ]
        , div [] [ text <| Debug.toString <| run duration "P" ]
        , div [] [ text <| Debug.toString <| run duration "Pabcd" ]
        , div [] [ text <| Debug.toString <| run duration "P0W" ]
        ]


type alias Duration =
    { years : Int
    , months : Int
    , weeks : Int
    , days : Int
    , hours : Int
    , minutes : Int
    , seconds : Float
    }


duration : Parser Duration
duration =
    oneOf
        [ (succeed Duration
            |. symbol "P"
            |= intParser "Y"
            |= intParser "M"
            |= intParser "W"
            |= intParser "D"
            |. symbol "T"
            |= intParser "H"
            |= intParser "M"
            |= floatParser "S"
          )
            |> backtrackable
        , succeed Duration
            |. symbol "P"
            |= intParser "Y"
            |= intParser "M"
            |= intParser "W"
            |= intParser "D"
            |= succeed 0
            |= succeed 0
            |= succeed 0
        ]


intParser : String -> Parser Int
intParser label =
    oneOf
        [ (succeed identity
            |= int
            |. symbol label
          )
            |> backtrackable
        , succeed 0
        ]


floatParser : String -> Parser Float
floatParser label =
    oneOf
        [ (succeed identity
            |= float
            |. symbol label
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
