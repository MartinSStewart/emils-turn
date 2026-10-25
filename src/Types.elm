module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Lamdera exposing (ClientId, SessionId)
import Url exposing (Url)


type Person
    = Emil
    | Martin
    | Jon


{-| How many fikas each person is ahead (positive) or behind (negative) by.
-}
type alias Balances =
    { emil : Int
    , martin : Int
    , jon : Int
    }


type alias FrontendModel =
    { key : Key
    , balances : Maybe Balances
    }


type alias BackendModel =
    { balances : Balances
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | PressedBuyFika Person (List Person)


type ToBackend
    = BuyFika Person (List Person)


type BackendMsg
    = OnConnect SessionId ClientId


type ToFrontend
    = CurrentBalances Balances


{-| The buyer gains one fika for each recipient, and each recipient owes one fika.
-}
buyFika : Person -> List Person -> Balances -> Balances
buyFika buyer recipients balances =
    List.foldl
        (\recipient acc -> addToBalance buyer 1 acc |> addToBalance recipient -1)
        balances
        (List.filter ((/=) buyer) recipients)


addToBalance : Person -> Int -> Balances -> Balances
addToBalance person amount balances =
    case person of
        Emil ->
            { balances | emil = balances.emil + amount }

        Martin ->
            { balances | martin = balances.martin + amount }

        Jon ->
            { balances | jon = balances.jon + amount }


getBalance : Person -> Balances -> Int
getBalance person balances =
    case person of
        Emil ->
            balances.emil

        Martin ->
            balances.martin

        Jon ->
            balances.jon
