module Main exposing (..)

----Draggable will be used to check the layout of current and furure elements

import Browser
import Browser.Events as Events exposing (Visibility(..))
import Draggable
import Html exposing (Html, a, br, button, div, h1, h2, hr, i, input, label, li, option, p, select, small, span, text, ul)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick, onInput)
import Http
import Http.Xml
import Json.Decode as D
import Url as U exposing (Url)
import Xml.Decode as X



---- MODEL ----


type alias Model =
    { websiteUrl : String
    , position : ( Int, Int )
    , drag : Draggable.State String
    , speedDetails : SpeedDetails
    , domainOwnershipDetails : DomainOwnershipDetails
    , isValid : Bool
    , showDomainDetails : Bool
    , domainSelected : Bool
    , speedSelected : Bool
    , stackSelected : Bool
    }


initialModel : Model
initialModel =
    { websiteUrl = ""
    , position = ( 0, 0 )
    , drag = Draggable.init
    , speedDetails = SpeedDetails "" ""
    , domainOwnershipDetails = DomainOwnershipDetails "" "" ""
    , isValid = False
    , showDomainDetails = False
    , domainSelected = False
    , speedSelected = False
    , stackSelected = False
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )



---- UPDATE ----


type Msg
    = ClickCheckWebsite
    | OnDragBy Draggable.Delta
    | DragMsg (Draggable.Msg String)
    | GotSpeed (Result Http.Error SpeedDetails)
    | GotDomain (Result Http.Error DomainOwnershipDetails)
    | UrlChange String
    | ExpandDomainContent
    | ApiSelectionChange Target Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickCheckWebsite ->
            ( model, Cmd.batch [ fetchFromWhoIsXML model model.websiteUrl, fetchFromGooglePageSpeedTest model model.websiteUrl ] )

        ExpandDomainContent ->
            ( { model | showDomainDetails = not model.showDomainDetails }, Cmd.none )

        UrlChange newUrl ->
            ( { model | websiteUrl = newUrl, isValid = checkWebsite newUrl }, Cmd.none )

        ApiSelectionChange target bool ->
            case target of
                TargetDomain ->
                    ( { model | domainSelected = not model.domainSelected }, Cmd.none )

                TargetSpeed ->
                    ( { model | speedSelected = not model.speedSelected }, Cmd.none )

                TargetStack ->
                    ( { model | stackSelected = not model.stackSelected }, Cmd.none )

        OnDragBy ( dx, dy ) ->
            let
                ( x, y ) =
                    model.position
            in
            ( { model | position = ( round (toFloat x + dx), round (toFloat y + dy) ) }, Cmd.none )

        GotSpeed result ->
            case result of
                Ok details ->
                    ( { model | websiteUrl = "Sucess", speedDetails = SpeedDetails details.timeToInteractive details.firstContentfulPaint }, Cmd.none )

                Err err ->
                    ( { model | websiteUrl = errorToString err }, Cmd.none )

        GotDomain result ->
            case result of
                Ok details ->
                    ( { model | websiteUrl = "Sucess", domainOwnershipDetails = DomainOwnershipDetails details.organization details.state details.country }, Cmd.none )

                Err err ->
                    ( { model | websiteUrl = errorToString err }, Cmd.none )

        DragMsg dragMsg ->
            Draggable.update dragConfig dragMsg model



---- Custom Types ----


type alias SpeedDetails =
    { timeToInteractive : String
    , firstContentfulPaint : String
    }


type alias DomainOwnershipDetails =
    { organization : String
    , state : String
    , country : String
    }


type Target
    = TargetDomain
    | TargetSpeed
    | TargetStack



---- Subscriptions ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Draggable.subscriptions DragMsg model.drag



---- Functions ----


checkWebsite : String -> Bool
checkWebsite websiteUrl =
    not (U.fromString websiteUrl == Nothing)


dragConfig : Draggable.Config String Msg
dragConfig =
    Draggable.basicConfig OnDragBy


renderSpeedDetails : SpeedDetails -> Html Msg
renderSpeedDetails sd =
    div [ class "output" ]
        [ text (String.concat [ "Time until interactive: ", sd.timeToInteractive, "First element loaded in: ", sd.firstContentfulPaint ]) ]


renderDomainDetails : DomainOwnershipDetails -> Html Msg
renderDomainDetails dod =
    div [ class "output" ]
        [ text (String.concat [ "Orginization: ", dod.organization, "State: ", dod.state, "Country: ", dod.country ]) ]


gpstDecoder : D.Decoder SpeedDetails
gpstDecoder =
    D.map2 SpeedDetails
        (D.field "lighthouseResult" (D.field "audits" (D.field "interactive" (D.field "displayValue" D.string))))
        (D.field "lighthouseResult" (D.field "audits" (D.field "first-contentful-paint" (D.field "displayValue" D.string))))


wixDecoder : X.Decoder DomainOwnershipDetails
wixDecoder =
    X.map3 DomainOwnershipDetails
        (X.path [ "registrant", "organization" ] (X.single X.string))
        (X.path [ "registrant", "state" ] (X.single X.string))
        (X.path [ "registrant", "country" ] (X.single X.string))


empty : Html msg
empty =
    Html.text ""


renderIf : Bool -> Html msg -> Html msg
renderIf shouldRender elem =
    if shouldRender then
        elem

    else
        empty


removeFromList : Int -> List a -> List a
removeFromList i xs =
    List.take i xs ++ List.drop (i + 1) xs



--- For some reason "d.at" doent work with D.at maybe look at it later


fetchFromGooglePageSpeedTest : Model -> String -> Cmd Msg
fetchFromGooglePageSpeedTest model websiteUrl =
    if model.speedSelected then
        Http.get
            { url = String.concat [ "https://www.googleapis.com/pagespeedonline/v5/runPagespeed?url=", websiteUrl, "&key=AIzaSyBfcmkhsGWVmLlVYn0YkTk6dDZFrcbbXV4&category=PERFORMANCE&strategy=DESKTOP" ]
            , expect = Http.expectJson GotSpeed gpstDecoder
            }

    else
        Cmd.none


fetchFromWhoIsXML : Model -> String -> Cmd Msg
fetchFromWhoIsXML model websiteUrl =
    if model.domainSelected then
        Http.get
            { url = String.concat [ "https://www.whoisxmlapi.com/whoisserver/WhoisService?apiKey=at_XRwFO1KDNvYMqdy0QfkAGpMhB7i58&domainName=", websiteUrl ]
            , expect = Http.Xml.expectXml GotDomain wixDecoder
            }

    else
        Cmd.none


errorToString : Http.Error -> String
errorToString error =
    case error of
        Http.BadUrl url ->
            "The URL " ++ url ++ " was invalid"

        Http.Timeout ->
            "Unable to reach the server, try again"

        Http.NetworkError ->
            "Unable to reach the server, check your network connection"

        Http.BadStatus 500 ->
            "The server had a problem, try again later"

        Http.BadStatus 400 ->
            "Verify your information and try again"

        Http.BadStatus _ ->
            "Unknown error"

        Http.BadBody errorMessage ->
            errorMessage



---- Constants ----
---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "main-section" ]
        [ div [ class "header" ]
            [ input [ class "urlInput", placeholder "", type_ "text", value model.websiteUrl, onInput UrlChange ]
                []
            , label []
                [ text "http://" ]
            , button [ class "startButton", onClick ClickCheckWebsite ]
                [ text "Check" ]
            , viewSelection model
            ]
        , viewDomain model
        , viewSpeed model
        , viewStack model
        ]


viewDomain : Model -> Html Msg
viewDomain model =
    div [ class "dashbord dashbord-domain" ]
        [ div [ class "detail-section" ]
            [ div [ class "general-info" ]
                [ h1 []
                    [ text "Domain" ]
                ]
            , div [ class "activate" ]
                [ label [ class "switch" ]
                    [ input [ type_ "checkbox", checked model.domainSelected, onCheck (ApiSelectionChange TargetDomain) ]
                        []
                    , span [ class "slider round" ]
                        []
                    ]
                ]
            , div [ class "status-info" ]
                [ h1 []
                    [ text "Status" ]
                ]
            , div [ class "expand-item" ]
                [ a [ class "arrowButton", onClick ExpandDomainContent ]
                    [ span [ class "leftSide" ]
                        []
                    , span [ class "rightSide" ]
                        []
                    ]
                ]
            ]
        , viewExpandDomain model |> renderIf model.showDomainDetails
        ]


viewExpandDomain : Model -> Html Msg
viewExpandDomain model =
    div [ class "domain-content-section" ]
        [ p []
            [ text (String.concat [ "Organization: ", model.domainOwnershipDetails.organization ]) ]
        , p []
            [ text (String.concat [ "State: ", model.domainOwnershipDetails.state ]) ]
        , p []
            [ text (String.concat [ "Country: ", model.domainOwnershipDetails.country ]) ]
        ]


viewSpeed : Model -> Html Msg
viewSpeed model =
    div [ class "dashbord dashbord-speed" ]
        [ div [ class "detail-section" ]
            [ div [ class "general-info" ]
                [ h1 []
                    [ text "Speed" ]
                ]
            , div [ class "activate" ]
                [ label [ class "switch" ]
                    [ input [ type_ "checkbox", checked model.speedSelected, onCheck (ApiSelectionChange TargetSpeed) ]
                        []
                    , span [ class "slider round" ]
                        []
                    ]
                ]
            , div [ class "status-info" ]
                [ h1 []
                    [ text "Status" ]
                ]
            , div [ class "expand-item" ]
                [ a [ class "arrowButton" ]
                    [ span [ class "leftSide" ]
                        []
                    , span [ class "rightSide" ]
                        []
                    ]
                ]
            ]
        ]


viewStack : Model -> Html Msg
viewStack model =
    div [ class "dashbord dashbord-stack" ]
        [ div [ class "detail-section" ]
            [ div [ class "general-info" ]
                [ h1 []
                    [ text "Stack" ]
                ]
            , div [ class "activate" ]
                [ label [ class "switch" ]
                    [ input [ type_ "checkbox", checked model.stackSelected, onCheck (ApiSelectionChange TargetStack) ]
                        []
                    , span [ class "slider round" ]
                        []
                    ]
                ]
            , div [ class "status-info" ]
                [ h1 []
                    [ text "Status" ]
                ]
            , div [ class "expand-item" ]
                [ a [ class "arrowButton" ]
                    [ span [ class "leftSide" ]
                        []
                    , span [ class "rightSide" ]
                        []
                    ]
                ]
            ]
        ]


viewSelection : Model -> Html Msg
viewSelection model =
    div [ class "selection-section" ]
        []



---- PROGRAM ----


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
