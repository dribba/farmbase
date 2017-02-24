module Utils.Http exposing (..)

import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)
import Http exposing (..)


postJson : Decoder value -> String -> Value -> Request value
postJson decoder url body =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Content-Type" "application/json" ]
        , url = url
        , body = Http.jsonBody body
        , expect = Http.expectJson decoder
        , timeout = Nothing
        , withCredentials = False
        }


putJson : Decoder value -> String -> Value -> Request value
putJson decoder url body =
    Http.request
        { method = "PUT"
        , headers = [ Http.header "Content-Type" "application/json" ]
        , url = url
        , body = Http.jsonBody body
        , expect = Http.expectJson decoder
        , timeout = Nothing
        , withCredentials = False
        }


httpDelete : Decoder value -> String -> Request value
httpDelete decoder url =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectJson decoder
        , timeout = Nothing
        , withCredentials = False
        }
