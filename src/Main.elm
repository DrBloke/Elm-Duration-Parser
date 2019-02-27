module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Parser exposing ((|.), (|=), Parser, andThen, backtrackable, end, float, int, oneOf, problem, run, succeed, symbol)


view : () -> Html msg
view model =
    div []
        [ div [] [ text <| "P4Y6M4DT12H30M5S" ++ " -- " ++ (Debug.toString <| run duration "P4Y6M4DT12H30M5S") ]
        , div [] [ text <| "P5D" ++ " -- " ++ (Debug.toString <| run duration "P5D") ]
        , div [] [ text <| "P5DT5M" ++ " -- " ++ (Debug.toString <| run duration "P5DT5M") ]
        , div [] [ text <| "PT5M" ++ " -- " ++ (Debug.toString <| run duration "PT5M") ]
        , div [] [ text <| "P5M" ++ " -- " ++ (Debug.toString <| run duration "P5M") ]
        , div [] [ text <| "P0W" ++ " -- " ++ (Debug.toString <| run duration "P0W") ]
        , div [] [ text <| "PT7.2S" ++ " -- " ++ (Debug.toString <| run duration "PT7.2S") ]
        , div [] [ text <| "P7.1W" ++ " -- " ++ (Debug.toString <| run duration "P7.1W") ]
        , div [] [ text <| "P" ++ " -- " ++ (Debug.toString <| run duration "P") ]
        , div [] [ text <| "Pabcd" ++ " -- " ++ (Debug.toString <| run duration "Pabcd") ]
        , div [] [ text <| "P5D5W" ++ " -- " ++ (Debug.toString <| run duration "P5D5W") ]
        , div [] [ text <| "5D5W" ++ " -- " ++ (Debug.toString <| run duration "5D5W") ]
        ]


type alias DurationWithMaybes =
    { years : Maybe Int
    , months : Maybe Int
    , weeks : Maybe Int
    , days : Maybe Int
    , hours : Maybe Int
    , minutes : Maybe Int
    , seconds : Maybe Float
    }


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
    succeed identity
        |. symbol "P"
        |= oneOf
            [ (succeed DurationWithMaybes
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
            , succeed DurationWithMaybes
                |= intParser "Y"
                |= intParser "M"
                |= intParser "W"
                |= intParser "D"
                |= succeed Nothing
                |= succeed Nothing
                |= succeed Nothing
            ]
        |. end
        |> andThen toDuration


toDuration : DurationWithMaybes -> Parser Duration
toDuration { years, months, weeks, days, hours, minutes, seconds } =
    if
        years
            == Nothing
            && months
            == Nothing
            && weeks
            == Nothing
            && days
            == Nothing
            && hours
            == Nothing
            && minutes
            == Nothing
            && seconds
            == Nothing
    then
        problem "Must have at least one value"

    else
        succeed
            { years = Maybe.withDefault 0 years
            , months = Maybe.withDefault 0 months
            , weeks = Maybe.withDefault 0 weeks
            , days = Maybe.withDefault 0 days
            , hours = Maybe.withDefault 0 hours
            , minutes = Maybe.withDefault 0 minutes
            , seconds = Maybe.withDefault 0 seconds
            }


intParser : String -> Parser (Maybe Int)
intParser label =
    oneOf
        [ (succeed Just
            |= int
            |. symbol label
          )
            |> backtrackable
        , succeed Nothing
        ]


floatParser : String -> Parser (Maybe Float)
floatParser label =
    oneOf
        [ (succeed Just
            |= float
            |. symbol label
          )
            |> backtrackable
        , succeed Nothing
        ]



--boilerplate


main : Program () () msg
main =
    Browser.sandbox
        { init = ()
        , view = view
        , update = \_ model -> model
        }
