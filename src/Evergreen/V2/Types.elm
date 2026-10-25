module Evergreen.V2.Types exposing (..)

import Browser
import Browser.Navigation
import Lamdera
import Url


type Person
    = Emil
    | Martin
    | Jon


type alias Balances =
    { emil : Int
    , martin : Int
    , jon : Int
    }


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , balances : Maybe Balances
    }


type alias BackendModel =
    { balances : Balances
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOpFrontendMsg
    | PressedBuyFika Person (List Person)


type ToBackend
    = BuyFika Person (List Person)


type BackendMsg
    = OnConnect Lamdera.SessionId Lamdera.ClientId


type ToFrontend
    = CurrentBalances Balances
