module Evergreen.V1.Types exposing (..)

import Browser
import Browser.Navigation
import Lamdera
import Url


type Turn
    = EmilsTurn
    | MartinsTurn


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , turn : Maybe Turn
    }


type alias BackendModel =
    { turn : Turn
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOpFrontendMsg
    | PressedEmilsTurn
    | PressedMartinsTurn


type ToBackend
    = ChangeTurn Turn


type BackendMsg
    = OnConnect Lamdera.SessionId Lamdera.ClientId


type ToFrontend
    = CurrentTurn Turn
