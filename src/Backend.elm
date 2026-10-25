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
    ( { balances = { emil = 0, martin = 0, jon = 0 } }
    , Cmd.none
    )


update : BackendMsg -> BackendModel -> ( BackendModel, Cmd BackendMsg )
update msg model =
    case msg of
        OnConnect _ clientId ->
            ( model, Lamdera.sendToFrontend clientId (CurrentBalances model.balances) )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg )
updateFromFrontend _ _ msg model =
    case msg of
        BuyFika buyer recipients ->
            let
                balances =
                    buyFika buyer recipients model.balances
            in
            ( { model | balances = balances }, Lamdera.broadcast (CurrentBalances balances) )
