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
    ( { purchases = [] }
    , Cmd.none
    )


update : BackendMsg -> BackendModel -> ( BackendModel, Cmd BackendMsg )
update msg model =
    case msg of
        OnConnect _ clientId ->
            ( model, Lamdera.sendToFrontend clientId (CurrentPurchases model.purchases) )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg )
updateFromFrontend _ _ msg model =
    let
        purchases =
            case msg of
                BoughtFika purchase ->
                    purchase :: model.purchases

                UndoLastPurchase ->
                    List.drop 1 model.purchases
    in
    ( { model | purchases = purchases }, Lamdera.broadcast (CurrentPurchases purchases) )
