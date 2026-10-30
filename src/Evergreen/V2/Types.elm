module Evergreen.V2.Types exposing (..)

import Browser
import Browser.Navigation
import Lamdera
import Url


type Person
    = Emil
    | Martin
    | Jon


type alias Purchase =
    { buyer : Person
    , recipients : List Person
    }


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , purchases : Maybe (List Purchase)
    }


type alias BackendModel =
    { purchases : List Purchase
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOpFrontendMsg
    | PressedBought Purchase
    | PressedUndo


type ToBackend
    = BoughtFika Purchase
    | UndoLastPurchase


type BackendMsg
    = OnConnect Lamdera.SessionId Lamdera.ClientId


type ToFrontend
    = CurrentPurchases (List Purchase)
