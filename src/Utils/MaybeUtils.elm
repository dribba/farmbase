module Utils.MaybeUtils exposing (..)

import Maybe exposing (withDefault)

or : Maybe a -> a -> a
or maybe def = 
    withDefault def maybe