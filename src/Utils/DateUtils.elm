module Utils.DateUtils exposing (formatDate, decodeDate)

import Date exposing (..)
import Json.Decode exposing (Decoder)
import Json.Decode as Decoder

toMonthNumber : Month -> Int
toMonthNumber month =
    case month of
        Jan ->
            1

        Feb ->
            2

        Mar ->
            3

        Apr ->
            4

        May ->
            5

        Jun ->
            6

        Jul ->
            7

        Aug ->
            8

        Sep ->
            9

        Oct ->
            10

        Nov ->
            11

        Dec ->
            12


asTwoDigits : Int -> String
asTwoDigits number =
    let
        prefix =
            if number < 10 then
                "0"
            else
                ""
    in
        prefix ++ toString number


formatDate : Date -> String
formatDate date =
    let
        year =
            date |> Date.year |> toString

        month =
            date |> Date.month |> toMonthNumber |> asTwoDigits

        day =
            date |> Date.day |> asTwoDigits
    in
        year ++ "-" ++ month ++ "-" ++ day

decodeDate : Decoder (Maybe Date)
decodeDate =
    Decoder.map (Date.fromString >> Result.toMaybe) Decoder.string