module Utils.Forms exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode


decodeInt =
    targetValue
        |> Decode.andThen strToInt


strToInt str =
    case String.toInt str of
        Ok i ->
            Decode.succeed i

        Err msg ->
            Decode.fail msg
