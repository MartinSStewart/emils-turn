module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Lamdera exposing (ClientId, SessionId)
import Url exposing (Url)


type Person
    = Emil
    | Martin
    | Jon


type alias Purchase =
    { buyer : Person
    , recipients : List Person
    }


type alias FrontendModel =
    { key : Key
    , purchases : Maybe (List Purchase)
    }


type alias BackendModel =
    { purchases : List Purchase
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | PressedBought Purchase
    | PressedUndo


type ToBackend
    = BoughtFika Purchase
    | UndoLastPurchase


type BackendMsg
    = OnConnect SessionId ClientId


type ToFrontend
    = CurrentPurchases (List Purchase)
