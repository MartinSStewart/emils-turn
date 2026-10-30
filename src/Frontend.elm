module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Html exposing (Html)
import Lamdera
import Svg
import Svg.Attributes
import Types exposing (..)
import Ui
import Ui.Font
import Ui.Input
import Ui.Prose
import Ui.Shadow
import Url


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = \m -> Sub.none
        , view = view
        }


init : Url.Url -> Nav.Key -> ( FrontendModel, Cmd FrontendMsg )
init url key =
    ( { key = key
      , purchases = Nothing
      }
    , Cmd.none
    )


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged url ->
            ( model, Cmd.none )

        NoOpFrontendMsg ->
            ( model, Cmd.none )

        PressedBought purchase ->
            ( model, Lamdera.sendToBackend (BoughtFika purchase) )

        PressedUndo ->
            ( model, Lamdera.sendToBackend UndoLastPurchase )


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        CurrentPurchases purchases ->
            ( { model | purchases = Just purchases }, Cmd.none )


people : List Person
people =
    [ Emil, Martin, Jon ]


personName : Person -> String
personName person =
    case person of
        Emil ->
            "Emil"

        Martin ->
            "Martin"

        Jon ->
            "Jon"


personIcon : Int -> Person -> Ui.Element msg
personIcon size person =
    case person of
        Emil ->
            Ui.html (rustLogo size)

        Martin ->
            Ui.image
                [ Ui.width (Ui.px (size * 42 // 32)) ]
                { source = "/elm-logo.png", description = "", onLoad = Nothing }

        Jon ->
            Ui.html (gavelIcon size)


{-| How many fikas a person is owed (positive) or owes (negative).
-}
balance : List Purchase -> Person -> Int
balance purchases person =
    List.foldl
        (\purchase total ->
            if purchase.buyer == person then
                total + List.length purchase.recipients

            else if List.member person purchase.recipients then
                total - 1

            else
                total
        )
        0
        purchases


{-| Every way one person can buy fika for the others: both of them, or just one.
-}
purchaseOptions : Person -> List Purchase
purchaseOptions buyer =
    let
        others =
            List.filter (\person -> person /= buyer) people
    in
    { buyer = buyer, recipients = others }
        :: List.map (\other -> { buyer = buyer, recipients = [ other ] }) others


purchaseText : String -> Purchase -> List (Ui.Element msg)
purchaseText verb purchase =
    personLabel purchase.buyer
        :: Ui.text (" " ++ verb ++ " ")
        :: (List.map personLabel purchase.recipients
                |> List.intersperse (Ui.text " och ")
           )


personLabel : Person -> Ui.Element msg
personLabel person =
    Ui.row
        [ Ui.width Ui.shrink ]
        [ Ui.el [ Ui.paddingRight 4 ] (personIcon 18 person)
        , Ui.el [ Ui.Font.bold ] (Ui.text (personName person))
        ]


view : FrontendModel -> Browser.Document FrontendMsg
view model =
    { title = ""
    , body =
        [ Ui.layout
            [ Ui.Font.family [ Ui.Font.sansSerif ]
            , Ui.height Ui.fill
            , Ui.padding 16
            , Ui.contentCenterY
            ]
            (case model.purchases of
                Just purchases ->
                    Ui.column
                        [ Ui.spacing 32, Ui.contentCenterX, Ui.widthMax 500, Ui.centerX ]
                        [ balancesView purchases
                        , Ui.column [ Ui.spacing 24 ] (List.map buyerButtons people)
                        , case purchases of
                            last :: _ ->
                                button PressedUndo
                                    (Ui.Prose.paragraph
                                        [ Ui.Font.size 16, Ui.Font.center ]
                                        (Ui.text "Ångra: " :: purchaseText "bjöd" last)
                                    )

                            [] ->
                                Ui.none
                        ]

                Nothing ->
                    Ui.none
            )
        ]
    }


balancesView : List Purchase -> Ui.Element msg
balancesView purchases =
    Ui.row
        [ Ui.spacing 8 ]
        (List.map
            (\person ->
                let
                    amount =
                        balance purchases person
                in
                Ui.column
                    [ Ui.spacing 4
                    , Ui.padding 12
                    , Ui.rounded 16
                    , Ui.border 1
                    , Ui.borderColor (Ui.rgb 200 200 200)
                    , Ui.contentCenterX
                    ]
                    [ Ui.el [ Ui.centerX, Ui.width Ui.shrink, Ui.height (Ui.px 32), Ui.contentCenterY ] (personIcon 24 person)
                    , Ui.el [ Ui.Font.center, Ui.Font.size 18 ] (Ui.text (personName person))
                    , Ui.el
                        [ Ui.Font.center
                        , Ui.Font.size 32
                        , Ui.Font.bold
                        , if amount > 0 then
                            Ui.Font.color (Ui.rgb 20 140 60)

                          else if amount < 0 then
                            Ui.Font.color (Ui.rgb 200 40 40)

                          else
                            Ui.Font.color (Ui.rgb 100 100 100)
                        ]
                        (Ui.text
                            (if amount > 0 then
                                "+" ++ String.fromInt amount

                             else
                                String.fromInt amount
                            )
                        )
                    ]
            )
            people
        )


buyerButtons : Person -> Ui.Element FrontendMsg
buyerButtons buyer =
    Ui.column
        [ Ui.spacing 8 ]
        (List.map
            (\purchase ->
                button (PressedBought purchase)
                    (Ui.Prose.paragraph
                        [ Ui.Font.size 20, Ui.Font.center ]
                        (purchaseText "bjuder" purchase)
                    )
            )
            (purchaseOptions buyer)
        )


button : msg -> Ui.Element msg -> Ui.Element msg
button onPress content =
    Ui.el
        [ Ui.Input.button onPress
        , Ui.rounded 16
        , Ui.padding 16
        , Ui.border 1
        , Ui.borderColor (Ui.rgb 100 100 100)
        , Ui.widthMax 500
        , Ui.Shadow.shadows
            [ { x = 0, y = 1, size = 0, blur = 4, color = Ui.rgba 0 0 0 0.1 } ]
        ]
        content


gavelIcon : Int -> Html msg
gavelIcon size =
    Svg.svg
        [ Svg.Attributes.version "1.1"
        , Svg.Attributes.height (String.fromInt size)
        , Svg.Attributes.width (String.fromInt size)
        , Svg.Attributes.viewBox "0 0 32 32"
        ]
        [ Svg.g
            [ Svg.Attributes.transform "rotate(-45 14 12)" ]
            [ Svg.rect
                [ Svg.Attributes.x "6"
                , Svg.Attributes.y "7"
                , Svg.Attributes.width "16"
                , Svg.Attributes.height "9"
                , Svg.Attributes.rx "2"
                , Svg.Attributes.fill "black"
                ]
                []
            , Svg.rect
                [ Svg.Attributes.x "4"
                , Svg.Attributes.y "6"
                , Svg.Attributes.width "3"
                , Svg.Attributes.height "11"
                , Svg.Attributes.rx "1"
                , Svg.Attributes.fill "black"
                ]
                []
            , Svg.rect
                [ Svg.Attributes.x "21"
                , Svg.Attributes.y "6"
                , Svg.Attributes.width "3"
                , Svg.Attributes.height "11"
                , Svg.Attributes.rx "1"
                , Svg.Attributes.fill "black"
                ]
                []
            , Svg.rect
                [ Svg.Attributes.x "12.5"
                , Svg.Attributes.y "16"
                , Svg.Attributes.width "3"
                , Svg.Attributes.height "16"
                , Svg.Attributes.rx "1.5"
                , Svg.Attributes.fill "black"
                ]
                []
            ]
        , Svg.rect
            [ Svg.Attributes.x "2"
            , Svg.Attributes.y "27"
            , Svg.Attributes.width "14"
            , Svg.Attributes.height "4"
            , Svg.Attributes.rx "1"
            , Svg.Attributes.fill "black"
            ]
            []
        ]


rustLogo : Int -> Html msg
rustLogo size =
    Svg.svg
        [ Svg.Attributes.version "1.1"
        , Svg.Attributes.height (String.fromInt size)
        , Svg.Attributes.width (String.fromInt size)
        , Svg.Attributes.viewBox "0 0 106 106"
        ]
        [ Svg.g
            [ Svg.Attributes.id "logo"
            , Svg.Attributes.transform "translate(53, 53)"
            ]
            [ Svg.path
                [ Svg.Attributes.id "r"
                , Svg.Attributes.transform "translate(0.5, 0.5)"
                , Svg.Attributes.stroke "black"
                , Svg.Attributes.strokeWidth "1"
                , Svg.Attributes.strokeLinejoin "round"
                , Svg.Attributes.d "\n        M -9,-15 H 4 C 12,-15 12,-7 4,-7 H -9 Z\n        M -40,22 H 0 V 11 H -9 V 3 H 1 C 12,3 6,22 15,22 H 40\n        V 3 H 34 V 5 C 34,13 25,12 24,7 C 23,2 19,-2 18,-2 C 33,-10 24,-26 12,-26 H -35\n        V -15 H -25 V 11 H -40 Z"
                ]
                []
            , Svg.g
                [ Svg.Attributes.id "gear"
                , Svg.Attributes.mask "url(#holes)"
                ]
                [ Svg.circle
                    [ Svg.Attributes.r "43"
                    , Svg.Attributes.fill "none"
                    , Svg.Attributes.stroke "black"
                    , Svg.Attributes.strokeWidth "9"
                    ]
                    []
                , Svg.g
                    [ Svg.Attributes.id "cogs"
                    ]
                    [ Svg.polygon
                        [ Svg.Attributes.id "cog"
                        , Svg.Attributes.stroke "black"
                        , Svg.Attributes.strokeWidth "3"
                        , Svg.Attributes.strokeLinejoin "round"
                        , Svg.Attributes.points "46,3 51,0 46,-3"
                        ]
                        []
                    , Svg.use
                        [ Svg.Attributes.xlinkHref "#cog"
                        , Svg.Attributes.transform "rotate(11.25)"
                        ]
                        []
                    , Svg.use
                        [ Svg.Attributes.xlinkHref "#cog"
                        , Svg.Attributes.transform "rotate(22.50)"
                        ]
                        []
                    , Svg.use
                        [ Svg.Attributes.xlinkHref "#cog"
                        , Svg.Attributes.transform "rotate(33.75)"
                        ]
                        []
                    , Svg.use
                        [ Svg.Attributes.xlinkHref "#cog"
                        , Svg.Attributes.transform "rotate(45.00)"
                        ]
                        []
                    , Svg.use
                        [ Svg.Attributes.xlinkHref "#cog"
                        , Svg.Attributes.transform "rotate(56.25)"
                        ]
                        []
                    , Svg.use
                        [ Svg.Attributes.xlinkHref "#cog"
                        , Svg.Attributes.transform "rotate(67.50)"
                        ]
                        []
                    , Svg.use
                        [ Svg.Attributes.xlinkHref "#cog"
                        , Svg.Attributes.transform "rotate(78.75)"
                        ]
                        []
                    , Svg.use
                        [ Svg.Attributes.xlinkHref "#cog"
                        , Svg.Attributes.transform "rotate(90.00)"
                        ]
                        []
                    , Svg.use
                        [ Svg.Attributes.xlinkHref "#cog"
                        , Svg.Attributes.transform "rotate(101.25)"
                        ]
                        []
                    , Svg.use
                        [ Svg.Attributes.xlinkHref "#cog"
                        , Svg.Attributes.transform "rotate(112.50)"
                        ]
                        []
                    , Svg.use
                        [ Svg.Attributes.xlinkHref "#cog"
                        , Svg.Attributes.transform "rotate(123.75)"
                        ]
                        []
                    , Svg.use
                        [ Svg.Attributes.xlinkHref "#cog"
                        , Svg.Attributes.transform "rotate(135.00)"
                        ]
                        []
                    , Svg.use
                        [ Svg.Attributes.xlinkHref "#cog"
                        , Svg.Attributes.transform "rotate(146.25)"
                        ]
                        []
                    , Svg.use
                        [ Svg.Attributes.xlinkHref "#cog"
                        , Svg.Attributes.transform "rotate(157.50)"
                        ]
                        []
                    , Svg.use
                        [ Svg.Attributes.xlinkHref "#cog"
                        , Svg.Attributes.transform "rotate(168.75)"
                        ]
                        []
                    , Svg.use
                        [ Svg.Attributes.xlinkHref "#cog"
                        , Svg.Attributes.transform "rotate(180.00)"
                        ]
                        []
                    , Svg.use
                        [ Svg.Attributes.xlinkHref "#cog"
                        , Svg.Attributes.transform "rotate(191.25)"
                        ]
                        []
                    , Svg.use
                        [ Svg.Attributes.xlinkHref "#cog"
                        , Svg.Attributes.transform "rotate(202.50)"
                        ]
                        []
                    , Svg.use
                        [ Svg.Attributes.xlinkHref "#cog"
                        , Svg.Attributes.transform "rotate(213.75)"
                        ]
                        []
                    , Svg.use
                        [ Svg.Attributes.xlinkHref "#cog"
                        , Svg.Attributes.transform "rotate(225.00)"
                        ]
                        []
                    , Svg.use
                        [ Svg.Attributes.xlinkHref "#cog"
                        , Svg.Attributes.transform "rotate(236.25)"
                        ]
                        []
                    , Svg.use
                        [ Svg.Attributes.xlinkHref "#cog"
                        , Svg.Attributes.transform "rotate(247.50)"
                        ]
                        []
                    , Svg.use
                        [ Svg.Attributes.xlinkHref "#cog"
                        , Svg.Attributes.transform "rotate(258.75)"
                        ]
                        []
                    , Svg.use
                        [ Svg.Attributes.xlinkHref "#cog"
                        , Svg.Attributes.transform "rotate(270.00)"
                        ]
                        []
                    , Svg.use
                        [ Svg.Attributes.xlinkHref "#cog"
                        , Svg.Attributes.transform "rotate(281.25)"
                        ]
                        []
                    , Svg.use
                        [ Svg.Attributes.xlinkHref "#cog"
                        , Svg.Attributes.transform "rotate(292.50)"
                        ]
                        []
                    , Svg.use
                        [ Svg.Attributes.xlinkHref "#cog"
                        , Svg.Attributes.transform "rotate(303.75)"
                        ]
                        []
                    , Svg.use
                        [ Svg.Attributes.xlinkHref "#cog"
                        , Svg.Attributes.transform "rotate(315.00)"
                        ]
                        []
                    , Svg.use
                        [ Svg.Attributes.xlinkHref "#cog"
                        , Svg.Attributes.transform "rotate(326.25)"
                        ]
                        []
                    , Svg.use
                        [ Svg.Attributes.xlinkHref "#cog"
                        , Svg.Attributes.transform "rotate(337.50)"
                        ]
                        []
                    , Svg.use
                        [ Svg.Attributes.xlinkHref "#cog"
                        , Svg.Attributes.transform "rotate(348.75)"
                        ]
                        []
                    ]
                , Svg.g
                    [ Svg.Attributes.id "mounts"
                    ]
                    [ Svg.polygon
                        [ Svg.Attributes.id "mount"
                        , Svg.Attributes.stroke "black"
                        , Svg.Attributes.strokeWidth "6"
                        , Svg.Attributes.strokeLinejoin "round"
                        , Svg.Attributes.points "-7,-42 0,-35 7,-42"
                        ]
                        []
                    , Svg.use
                        [ Svg.Attributes.xlinkHref "#mount"
                        , Svg.Attributes.transform "rotate(72)"
                        ]
                        []
                    , Svg.use
                        [ Svg.Attributes.xlinkHref "#mount"
                        , Svg.Attributes.transform "rotate(144)"
                        ]
                        []
                    , Svg.use
                        [ Svg.Attributes.xlinkHref "#mount"
                        , Svg.Attributes.transform "rotate(216)"
                        ]
                        []
                    , Svg.use
                        [ Svg.Attributes.xlinkHref "#mount"
                        , Svg.Attributes.transform "rotate(288)"
                        ]
                        []
                    ]
                ]
            , Svg.mask
                [ Svg.Attributes.id "holes"
                ]
                [ Svg.rect
                    [ Svg.Attributes.x "-60"
                    , Svg.Attributes.y "-60"
                    , Svg.Attributes.width "120"
                    , Svg.Attributes.height "120"
                    , Svg.Attributes.fill "white"
                    ]
                    []
                , Svg.circle
                    [ Svg.Attributes.id "hole"
                    , Svg.Attributes.cy "-40"
                    , Svg.Attributes.r "3"
                    ]
                    []
                , Svg.use
                    [ Svg.Attributes.xlinkHref "#hole"
                    , Svg.Attributes.transform "rotate(72)"
                    ]
                    []
                , Svg.use
                    [ Svg.Attributes.xlinkHref "#hole"
                    , Svg.Attributes.transform "rotate(144)"
                    ]
                    []
                , Svg.use
                    [ Svg.Attributes.xlinkHref "#hole"
                    , Svg.Attributes.transform "rotate(216)"
                    ]
                    []
                , Svg.use
                    [ Svg.Attributes.xlinkHref "#hole"
                    , Svg.Attributes.transform "rotate(288)"
                    ]
                    []
                ]
            ]
        ]
