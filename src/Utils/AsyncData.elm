module Utils.AsyncData exposing (..)


type AsyncData error data
    = Empty
    | Pending
    | PendingStale data
    | Ready data
    | Failed error
    | FailedStale error data
    | Unavailable


pending : AsyncData error data -> AsyncData error data
pending async =
    case async of
        Ready data ->
            PendingStale data

        FailedStale error data ->
            PendingStale data

        _ ->
            Pending


fail : AsyncData error data -> error -> AsyncData error data
fail async error =
    case async of
        PendingStale data ->
            FailedStale error data

        _ ->
            Failed error


isEmpty : AsyncData e d -> Bool
isEmpty async =
    async == Empty


nonEmpty : AsyncData e d -> Bool
nonEmpty =
    not << isEmpty


isPending : AsyncData e d -> Bool
isPending async =
    case async of
        Pending ->
            True

        PendingStale _ ->
            True

        _ ->
            False


isStale : AsyncData e d -> Bool
isStale async =
    case async of
        FailedStale _ _ ->
            True

        PendingStale _ ->
            True

        _ ->
            False


isFailed : AsyncData e d -> Bool
isFailed async =
    case async of
        Failed _ ->
            True

        FailedStale _ _ ->
            True

        _ ->
            False


isReady : AsyncData e d -> Bool
isReady async =
    case async of
        Ready _ ->
            True

        _ ->
            False


isUnavailable : AsyncData e d -> Bool
isUnavailable async =
    case async of
        Unavailable ->
            True

        _ ->
            False
