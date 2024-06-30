module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Html exposing (Html)
import Html.Attributes as Attr
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
      , turn = Nothing
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

        PressedEmilsTurn ->
            ( { model | turn = Just EmilsTurn }, Lamdera.sendToBackend (ChangeTurn EmilsTurn) )

        PressedMartinsTurn ->
            ( { model | turn = Just MartinsTurn }, Lamdera.sendToBackend (ChangeTurn MartinsTurn) )


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        CurrentTurn turn ->
            ( { model | turn = Just turn }, Cmd.none )


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
            (Ui.column
                []
                [ Ui.el
                    []
                    (case model.turn of
                        Just EmilsTurn ->
                            Ui.column
                                [ Ui.spacing 32, Ui.contentCenterX ]
                                [ title (Ui.html rustLogo) "Emil"
                                , button
                                    PressedMartinsTurn
                                    (Ui.Prose.paragraph
                                        [ Ui.Font.size 32, Ui.Font.center ]
                                        [ Ui.text "Nej, nu är det "
                                        , Ui.el [ Ui.Font.bold ] (Ui.text "Martin")
                                        , Ui.text "'s\u{00A0}tur"
                                        ]
                                    )
                                ]

                        Just MartinsTurn ->
                            Ui.column
                                [ Ui.spacing 32, Ui.contentCenterX ]
                                [ title
                                    (Ui.image
                                        [ Ui.width (Ui.px 42) ]
                                        { source = "/elm-logo.png", description = "", onLoad = Nothing }
                                    )
                                    "Martin"
                                , button PressedEmilsTurn
                                    (Ui.Prose.paragraph
                                        [ Ui.Font.size 32, Ui.Font.center ]
                                        [ Ui.text "Nej, nu är det "
                                        , Ui.el [ Ui.Font.bold ] (Ui.text "Emil")
                                        , Ui.text "'s\u{00A0}tur"
                                        ]
                                    )
                                ]

                        Nothing ->
                            Ui.none
                    )
                ]
            )
        ]
    }


title logo name =
    Ui.column
        [ Ui.centerX, Ui.Font.size 44, Ui.Font.center, Ui.Font.lineHeight 1.2 ]
        [ Ui.text "Nu är det "
        , Ui.row
            [ Ui.centerX, Ui.Font.size 60 ]
            [ logo, Ui.el [ Ui.Font.bold, Ui.width Ui.shrink ] (Ui.text name), Ui.text "'s" ]
        , Ui.text " tur att köpa\u{00A0}fika!"
        ]


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


rustLogo : Html msg
rustLogo =
    Svg.svg
        [ Svg.Attributes.version "1.1"
        , Svg.Attributes.height "32"
        , Svg.Attributes.width "32"
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
