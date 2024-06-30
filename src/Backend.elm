module Backend exposing (..)

import Lamdera exposing (ClientId, SessionId)
import Types exposing (..)


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = \m -> Lamdera.onConnect OnConnect
        }


init : ( BackendModel, Cmd BackendMsg )
init =
    ( { turn = MartinsTurn }
    , Cmd.none
    )


update : BackendMsg -> BackendModel -> ( BackendModel, Cmd BackendMsg )
update msg model =
    case msg of
        OnConnect _ clientId ->
            ( model, Lamdera.sendToFrontend clientId (CurrentTurn model.turn) )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg )
updateFromFrontend _ _ msg model =
    case msg of
        ChangeTurn turn ->
            ( { model | turn = turn }, Lamdera.broadcast (CurrentTurn turn) )
