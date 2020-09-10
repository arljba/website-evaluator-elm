module Main exposing (..)

----Draggable will be used to check the layout of current and furure elements

import Browser
import Browser.Events as Events exposing (Visibility(..))
import Draggable
import Html exposing (Html, a, br, button, div, h1, h2, hr, i, input, label, li, option, p, select, small, span, text, ul)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick, onInput)
import Http exposing (Header, emptyBody)
import Http.Xml
import Json.Decode as D
import Url as U exposing (Url)
import Xml.Decode as X



---- MODEL ----


type alias Model =
    { input : String
    , websiteUrl : Maybe Url
    , position : ( Int, Int )
    , drag : Draggable.State String
    , speedDetails : SpeedDetails
    , domainOwnershipDetails : DomainOwnershipDetails
    , stackDetails : StackDetails
    , structDetails : StructureDetails
    , isValid : Bool
    , showDomainDetails : Bool
    , showStackDetails : Bool
    , showStructDetails : Bool
    , apiSelection : ApiSelection
    , domainStatus : String
    , speedStatus : String
    , stackStatus : String
    , structStatus : String
    }


initialModel : Model
initialModel =
    { input = ""
    , websiteUrl = Nothing
    , position = ( 0, 0 )
    , drag = Draggable.init
    , speedDetails = SpeedDetails "" ""
    , domainOwnershipDetails = DomainOwnershipDetails "" "" ""
    , stackDetails = StackDetails []
    , structDetails = StructureDetails []
    , isValid = False
    , showDomainDetails = False
    , showStackDetails = False
    , showStructDetails = False
    , apiSelection = ApiSelection False False False False False
    , domainStatus = "Status"
    , speedStatus = "Status"
    , stackStatus = "Status"
    , structStatus = "Status"
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
    | GotStack (Result Http.Error StackDetails)
    | GotStructure (Result Http.Error StructureDetails)
    | UrlChange String
    | ExpandDomainContent
    | ExpandStackContent
    | ExpandStructContent
    | ApiSelectionChange Target Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickCheckWebsite ->
            ( model, Cmd.batch [ fetchFromWhoIsXML model model.websiteUrl, fetchFromGooglePageSpeedTest model model.websiteUrl, fetchFromBuiltwith model model.websiteUrl, fetchFromCrawler model model.websiteUrl ] )

        ExpandDomainContent ->
            ( { model | showDomainDetails = not model.showDomainDetails }, Cmd.none )

        ExpandStackContent ->
            ( { model | showStackDetails = not model.showStackDetails }, Cmd.none )

        ExpandStructContent ->
            ( { model | showStructDetails = not model.showStructDetails }, Cmd.none )

        UrlChange newInput ->
            ( { model | input = newInput, websiteUrl = U.fromString newInput }, Cmd.none )

        ApiSelectionChange target bool ->
            case target of
                TargetDomain ->
                    ( { model | apiSelection = toggleDomainSelected model.apiSelection }, Cmd.none )

                TargetSpeed ->
                    ( { model | apiSelection = toggleSpeedSelected model.apiSelection }, Cmd.none )

                TargetStack ->
                    ( { model | apiSelection = toggleStackSelected model.apiSelection }, Cmd.none )

                TargetLink ->
                    ( { model | apiSelection = toggleLinkSelected model.apiSelection }, Cmd.none )

                TargetStruct ->
                    ( { model | apiSelection = toggleStructSelected model.apiSelection }, Cmd.none )

        OnDragBy ( dx, dy ) ->
            let
                ( x, y ) =
                    model.position
            in
            ( { model | position = ( round (toFloat x + dx), round (toFloat y + dy) ) }, Cmd.none )

        GotSpeed result ->
            case result of
                Ok details ->
                    ( { model | speedStatus = "Sucess", speedDetails = SpeedDetails details.timeToInteractive details.firstContentfulPaint }, Cmd.none )

                Err err ->
                    ( { model | speedStatus = errorToString err }, Cmd.none )

        GotDomain result ->
            case result of
                Ok details ->
                    ( { model | domainStatus = "Sucess", domainOwnershipDetails = DomainOwnershipDetails details.organization details.state details.country }, Cmd.none )

                Err err ->
                    ( { model | domainStatus = errorToString err }, Cmd.none )

        GotStack result ->
            case result of
                Ok details ->
                    ( { model | stackStatus = "Sucess", stackDetails = StackDetails details.technologies }, Cmd.none )

                Err err ->
                    ( { model | stackStatus = errorToString err }, Cmd.none )

        GotStructure result ->
            case result of
                Ok details ->
                    ( { model | structStatus = "Sucess", structDetails = StructureDetails details.items }, Cmd.none )

                Err err ->
                    ( { model | structStatus = errorToString err }, Cmd.none )

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


type alias StackDetails =
    { technologies : List Technologie
    }


type alias Technologie =
    { name : String
    , categories : String
    }


type alias StructureDetails =
    { items : List StructureItem }


type alias StructureItem =
    { source : String
    , dest : String
    , status : Int
    }


type Target
    = TargetDomain
    | TargetSpeed
    | TargetStack
    | TargetLink
    | TargetStruct


type alias ApiSelection =
    { domainSelected : Bool
    , speedSelected : Bool
    , stackSelected : Bool
    , linkSelected : Bool
    , structureSelected : Bool
    }


toggleDomainSelected : ApiSelection -> ApiSelection
toggleDomainSelected selection =
    { selection | domainSelected = not selection.domainSelected }


toggleSpeedSelected : ApiSelection -> ApiSelection
toggleSpeedSelected selection =
    { selection | speedSelected = not selection.speedSelected }


toggleStackSelected : ApiSelection -> ApiSelection
toggleStackSelected selection =
    { selection | stackSelected = not selection.stackSelected }


toggleLinkSelected : ApiSelection -> ApiSelection
toggleLinkSelected selection =
    { selection | linkSelected = not selection.linkSelected }


toggleStructSelected : ApiSelection -> ApiSelection
toggleStructSelected selection =
    { selection | structureSelected = not selection.structureSelected }



---- Subscriptions ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Draggable.subscriptions DragMsg model.drag



---- Functions ----


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


extractWapDecoder : D.Decoder StackDetails
extractWapDecoder =
    D.field "Results" (D.index 0 (D.at [ "Result", "Paths" ] (D.index 0 wapDecoder)))


wapDecoder : D.Decoder StackDetails
wapDecoder =
    D.map StackDetails
        (D.field "Technologies" (D.list techDecoder))


techDecoder : D.Decoder Technologie
techDecoder =
    D.map2 Technologie
        (D.field "Name" D.string)
        (D.field "Tag" D.string)


crawlDecoder : D.Decoder StructureDetails
crawlDecoder =
    D.map StructureDetails
        (D.field "items" (D.list itemDecoder))


itemDecoder : D.Decoder StructureItem
itemDecoder =
    D.map3 StructureItem
        (D.field "url_src" D.string)
        (D.field "url_dest" D.string)
        (D.field "status" D.int)


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


fetchFromGooglePageSpeedTest : Model -> Maybe Url -> Cmd Msg
fetchFromGooglePageSpeedTest model websiteUrl =
    case websiteUrl of
        Just url ->
            if model.apiSelection.speedSelected then
                Http.get
                    { url = String.concat [ "https://www.googleapis.com/pagespeedonline/v5/runPagespeed?url=", U.toString url, "&key=AIzaSyBfcmkhsGWVmLlVYn0YkTk6dDZFrcbbXV4&category=PERFORMANCE&strategy=DESKTOP" ]
                    , expect = Http.expectJson GotSpeed gpstDecoder
                    }

            else
                Cmd.none

        Nothing ->
            Cmd.none


fetchFromWhoIsXML : Model -> Maybe Url -> Cmd Msg
fetchFromWhoIsXML model websiteUrl =
    case websiteUrl of
        Just url ->
            if model.apiSelection.domainSelected then
                Http.get
                    { url = String.concat [ "https://www.whoisxmlapi.com/whoisserver/WhoisService?apiKey=at_XRwFO1KDNvYMqdy0QfkAGpMhB7i58&domainName=", U.toString url ]
                    , expect = Http.Xml.expectXml GotDomain wixDecoder
                    }

            else
                Cmd.none

        Nothing ->
            Cmd.none


fetchFromBuiltwith : Model -> Maybe Url -> Cmd Msg
fetchFromBuiltwith model websiteUrl =
    case websiteUrl of
        Just url ->
            if model.apiSelection.stackSelected then
                Http.get
                    { url = String.concat [ "https://api.builtwith.com/v17/api.json?KEY=8e1d176e-26be-4379-8f60-79d46a255c0d&LOOKUP=", U.toString url, "&HIDEDL=no" ]
                    , expect = Http.expectJson GotStack extractWapDecoder
                    }

            else
                Cmd.none

        Nothing ->
            Cmd.none


fetchFromCrawler : Model -> Maybe Url -> Cmd Msg
fetchFromCrawler model websiteUrl =
    case websiteUrl of
        Just url ->
            if model.apiSelection.structureSelected then
                Http.get
                    { url = String.concat [ "http://arne-baumann.de:9080/crawl.json?spider_name=linkspider&start_requests=true&domain=", url.host, "&starturl=", U.toString url ]
                    , expect = Http.expectJson GotStructure crawlDecoder
                    }

            else
                Cmd.none

        Nothing ->
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


renderTechnologies : Technologie -> Html Msg
renderTechnologies tech =
    div [ class "card" ]
        [ div [ class "card-header" ]
            [ text tech.categories ]
        , div [ class "card-main" ]
            [ div [ class "main-description" ]
                [ text tech.name ]
            ]
        ]



---- Constants ----
---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "main-section" ]
        [ div [ class "header" ]
            [ input [ class "urlInput", placeholder "", type_ "text", value model.input, onInput UrlChange ]
                []
            , label []
                [ text "http://" ]
            , button [ class "startButton", onClick ClickCheckWebsite ]
                [ text "Check" ]
            , viewSelection model
            ]
        , viewInfo model
        , viewDomain model
        , viewSpeed model
        , viewStack model
        , viewLink model
        , viewStructure model
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
                    [ input [ type_ "checkbox", checked model.apiSelection.domainSelected, onCheck (ApiSelectionChange TargetDomain) ]
                        []
                    , span [ class "slider round" ]
                        []
                    ]
                ]
            , div [ class "status-info" ]
                [ h1 []
                    [ text model.domainStatus ]
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
                    [ input [ type_ "checkbox", checked model.apiSelection.speedSelected, onCheck (ApiSelectionChange TargetSpeed) ]
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
                    [ input [ type_ "checkbox", checked model.apiSelection.stackSelected, onCheck (ApiSelectionChange TargetStack) ]
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
                [ a [ class "arrowButton", onClick ExpandStackContent ]
                    [ span [ class "leftSide" ]
                        []
                    , span [ class "rightSide" ]
                        []
                    ]
                ]
            ]
        , viewExpandStack model |> renderIf model.showStackDetails
        ]


viewExpandStack : Model -> Html Msg
viewExpandStack model =
    div [ class "stack-content-section" ]
        [ div [ class "cards" ]
            (List.map
                renderTechnologies
                model.stackDetails.technologies
            )
        ]


viewInfo : Model -> Html Msg
viewInfo model =
    div [ class "dashbord dashbord-info" ]
        [ div [ class "detail-section" ]
            [ div [ class "head-general-info" ]
                [ h1 []
                    [ text "API" ]
                ]
            , div [ class "head-activate" ]
                [ h1 []
                    [ text "Active" ]
                ]
            , div [ class "head-status-info" ]
                [ h1 []
                    [ text "Status" ]
                ]
            , div [ class "head-expand-item" ]
                [ h1 []
                    [ text "Details" ]
                ]
            ]
        ]


viewLink : Model -> Html Msg
viewLink model =
    div [ class "dashbord dashbord-speed" ]
        [ div [ class "detail-section" ]
            [ div [ class "general-info" ]
                [ h1 []
                    [ text "Link" ]
                ]
            , div [ class "activate" ]
                [ label [ class "switch" ]
                    [ input [ type_ "checkbox", checked model.apiSelection.linkSelected, onCheck (ApiSelectionChange TargetLink) ]
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


viewStructure : Model -> Html Msg
viewStructure model =
    div [ class "dashbord dashbord-speed" ]
        [ div [ class "detail-section" ]
            [ div [ class "general-info" ]
                [ h1 []
                    [ text "Structure" ]
                ]
            , div [ class "activate" ]
                [ label [ class "switch" ]
                    [ input [ type_ "checkbox", checked model.apiSelection.structureSelected, onCheck (ApiSelectionChange TargetStruct) ]
                        []
                    , span [ class "slider round" ]
                        []
                    ]
                ]
            , div [ class "status-info" ]
                [ h1 []
                    [ text model.structStatus ]
                ]
            , div [ class "expand-item" ]
                [ a [ class "arrowButton", onClick ExpandStructContent ]
                    [ span [ class "leftSide" ]
                        []
                    , span [ class "rightSide" ]
                        []
                    ]
                ]
            ]
        , viewExpandStruct model |> renderIf model.showStructDetails
        ]


viewExpandStruct : Model -> Html Msg
viewExpandStruct model =
    empty


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
