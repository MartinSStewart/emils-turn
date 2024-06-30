module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Lamdera exposing (ClientId, SessionId)
import Url exposing (Url)


type Turn
    = EmilsTurn
    | MartinsTurn


type alias FrontendModel =
    { key : Key
    , turn : Maybe Turn
    }


type alias BackendModel =
    { turn : Turn
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | PressedEmilsTurn
    | PressedMartinsTurn


type ToBackend
    = ChangeTurn Turn


type BackendMsg
    = OnConnect SessionId ClientId


type ToFrontend
    = CurrentTurn Turn
