module Main exposing (..)

----Draggable will be used to check the layout of current and furure elements

import Array exposing (append)
import Browser
import Browser.Events as Events exposing (Visibility(..))
import Dict exposing (Dict)
import ForceDirectedGraph as FDG
import Graph exposing (Edge, Graph, Node, NodeId)
import Html exposing (Html, a, b, br, button, div, h1, h2, hr, i, img, input, label, li, option, p, select, small, span, text, ul)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick, onInput)
import Http exposing (Header, emptyBody)
import Http.Xml
import Json.Decode as D
import Round as R
import Url as U exposing (Url)
import Xml.Decode as X



---- MODEL ----


type alias Model =
    { input : String
    , websiteUrl : Maybe Url
    , position : ( Int, Int )
    , speedDetails : SpeedDetails
    , domainOwnershipDetails : DomainOwnershipDetails
    , stackDetails : StackDetails
    , structDetails : StructureDetails
    , isValid : Bool
    , showDomainDetails : Bool
    , showSpeedDetails : Bool
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
    , speedDetails = SpeedDetails 0 0 0
    , domainOwnershipDetails = DomainOwnershipDetails "" "" ""
    , stackDetails = StackDetails []
    , structDetails = StructureDetails []
    , isValid = False
    , showDomainDetails = False
    , showSpeedDetails = False
    , showStackDetails = False
    , showStructDetails = False
    , apiSelection = ApiSelection False False False False
    , domainStatus = "Not Started"
    , speedStatus = "Not Started"
    , stackStatus = "Not Started"
    , structStatus = "Not Started"
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )



---- UPDATE ----


type Msg
    = ClickCheckWebsite
    | GotSpeed (Result Http.Error SpeedDetails)
    | GotDomain (Result Http.Error DomainOwnershipDetails)
    | GotStack (Result Http.Error StackDetails)
    | GotStructure (Result Http.Error StructureDetails)
    | UrlChange String
    | ExpandDomainContent
    | ExpandSpeedContent
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

        ExpandSpeedContent ->
            ( { model | showSpeedDetails = not model.showSpeedDetails }, Cmd.none )

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

                TargetStruct ->
                    ( { model | apiSelection = toggleStructSelected model.apiSelection }, Cmd.none )

        GotSpeed result ->
            case result of
                Ok details ->
                    ( { model | speedStatus = "Sucess", speedDetails = SpeedDetails details.timeToInteractive details.firstContentfulPaint details.serverResponseTime }, Cmd.none )

                Err err ->
                    ( { model | speedStatus = "Error" }, Cmd.none )

        GotDomain result ->
            case result of
                Ok details ->
                    ( { model | domainStatus = "Sucess", domainOwnershipDetails = DomainOwnershipDetails details.organization details.state details.country }, Cmd.none )

                Err err ->
                    ( { model | domainStatus = "Error" }, Cmd.none )

        GotStack result ->
            case result of
                Ok details ->
                    ( { model | stackStatus = "Sucess", stackDetails = StackDetails details.technologies }, Cmd.none )

                Err err ->
                    ( { model | stackStatus = "Error" }, Cmd.none )

        GotStructure result ->
            case result of
                Ok details ->
                    ( { model | structStatus = "Sucess", structDetails = StructureDetails details.items }, Cmd.none )

                Err err ->
                    ( { model | structStatus = "Error" }, Cmd.none )



---- Custom Types ----


type alias SpeedDetails =
    { timeToInteractive : Float
    , firstContentfulPaint : Float
    , serverResponseTime : Float
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
    | TargetStruct


type alias ApiSelection =
    { domainSelected : Bool
    , speedSelected : Bool
    , stackSelected : Bool
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


toggleStructSelected : ApiSelection -> ApiSelection
toggleStructSelected selection =
    { selection | structureSelected = not selection.structureSelected }



---- Subscriptions ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



---- Functions ----


gpstDecoder : D.Decoder SpeedDetails
gpstDecoder =
    D.map3 SpeedDetails
        (D.field "lighthouseResult" (D.field "audits" (D.field "interactive" (D.field "numericValue" D.float))))
        (D.field "lighthouseResult" (D.field "audits" (D.field "first-contentful-paint" (D.field "numericValue" D.float))))
        (D.field "lighthouseResult" (D.field "audits" (D.field "server-response-time" (D.field "numericValue" D.float))))


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

        Http.BadBody _ ->
            "Cant parse body"


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



-------------------List-----------------------------


createListFromItems : List StructureItem -> List String
createListFromItems items =
    List.append (List.map (\record -> record.source) items) (List.map (\record -> record.dest) items)


createUniqueList : List String -> List String
createUniqueList list =
    case list of
        [] ->
            []

        x :: [] ->
            x :: []

        x :: xs ->
            if List.member x xs then
                createUniqueList xs

            else
                x :: createUniqueList xs


createTupelList : List a -> List ( a, Int )
createTupelList items =
    List.indexedMap (\i x -> ( x, i )) items


createDict : List String -> Dict String Int
createDict list =
    Dict.fromList (createTupelList list)


createLinks : List StructureItem -> Dict String Int -> List ( Maybe Int, Maybe Int )
createLinks list dict =
    List.map (\record -> ( Dict.get record.source dict, Dict.get record.dest dict )) list


extractMaybebVal : List ( Maybe Int, Maybe Int ) -> List ( Int, Int )
extractMaybebVal list =
    List.foldr
        (\mbVal acc ->
            case mbVal of
                ( first, second ) ->
                    case ( first, second ) of
                        ( Just f, Just s ) ->
                            if f == s then
                                acc

                            else
                                ( f, s ) :: acc

                        ( _, Nothing ) ->
                            acc

                        ( Nothing, _ ) ->
                            acc
        )
        []
        list



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
        [ div [ class "card" ]
            [ img [ alt "Image", src "../images/persons-solid.svg" ]
                []
            , div [ class "container" ]
                [ h2 []
                    [ b []
                        [ text "Organization" ]
                    ]
                , p []
                    [ text model.domainOwnershipDetails.organization ]
                ]
            ]
        , div [ class "card" ]
            [ img [ alt "Image", src "../images/map-solid.svg" ]
                []
            , div [ class "container" ]
                [ h2 []
                    [ b []
                        [ text "State" ]
                    ]
                , p []
                    [ text model.domainOwnershipDetails.state ]
                ]
            ]
        , div [ class "card" ]
            [ img [ alt "Image", src "../images/globe-solid.svg" ]
                []
            , div [ class "container" ]
                [ h2 []
                    [ b []
                        [ text "Country" ]
                    ]
                , p []
                    [ text model.domainOwnershipDetails.country ]
                ]
            ]
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
                    [ text model.speedStatus ]
                ]
            , div [ class "expand-item" ]
                [ a [ class "arrowButton", onClick ExpandSpeedContent ]
                    [ span [ class "leftSide" ]
                        []
                    , span [ class "rightSide" ]
                        []
                    ]
                ]
            ]
        , viewExpandSpeed model |> renderIf model.showSpeedDetails
        ]


viewExpandSpeed : Model -> Html Msg
viewExpandSpeed model =
    div [ class "speed-content-section" ]
        [ div [ class "card" ]
            [ img [ alt "Image", src "../images/server-solid.svg" ]
                []
            , div [ class "container" ]
                [ h2 []
                    [ b []
                        [ text "Server Responstime" ]
                    ]
                , p []
                    [ text (String.concat [ R.round 2 (model.speedDetails.serverResponseTime / 1000), " Sekunden" ]) ]
                ]
            ]
        , div [ class "card" ]
            [ img [ alt "Image", src "../images/paint-brush-solid.svg" ]
                []
            , div [ class "container" ]
                [ h2 []
                    [ b []
                        [ text "Time untill first content is drawn" ]
                    ]
                , p []
                    [ text (String.concat [ R.round 2 (model.speedDetails.firstContentfulPaint / 1000), " Sekunden" ]) ]
                ]
            ]
        , div [ class "card" ]
            [ img [ alt "Image", src "../images/mouse-solid.svg" ]
                []
            , div [ class "container" ]
                [ h2 []
                    [ b []
                        [ text "Time untill site is interactive" ]
                    ]
                , p []
                    [ text (String.concat [ R.round 2 (model.speedDetails.timeToInteractive / 1000), " Sekunden" ]) ]
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
                    [ text model.stackStatus ]
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
    FDG.initGraph (createGraph model.structDetails.items) |> FDG.viewGraph


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


createGraph : List StructureItem -> Graph String ()
createGraph items =
    let
        labels =
            createUniqueList (createListFromItems items)

        dict =
            createDict (createUniqueList (createListFromItems items))
    in
    Graph.fromNodeLabelsAndEdgePairs labels (extractMaybebVal (createLinks items dict))
