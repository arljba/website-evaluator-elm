module Main exposing (main)

----Draggable will be used to check the layout of current and furure elements

import Api exposing (..)
import Array exposing (append)
import Browser
import Dict exposing (Dict)
import ForceDirectedGraph exposing (initGraph, viewGraph)
import Graph exposing (Graph)
import Html exposing (Html, a, b, button, div, h1, h2, img, input, label, p, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick, onInput)
import Http exposing (Error)
import ListHelper exposing (..)
import Round as R
import Types exposing (..)
import Url as U exposing (Url)
import Xml.Decode as X



---- MODEL ----


initialModel : Model
initialModel =
    { input = ""
    , websiteUrl = Nothing
    , speedDetails = SpeedDetails 0 0 0
    , domainOwnershipDetails = DomainOwnershipDetails "" "" ""
    , stackDetails = StackDetails []
    , structDetails = StructureDetails []
    , isValidUrl = False
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickCheckWebsite ->
            ( model, Cmd.batch [ fetchFromWhoIsXML model model.websiteUrl, fetchFromGooglePageSpeedTest model model.websiteUrl, fetchFromBuiltwith model model.websiteUrl, fetchFromCrawler model model.websiteUrl ] )

        ClearModel ->
            ( initialModel, Cmd.none )

        ExpandDomainContent ->
            ( { model | showDomainDetails = not model.showDomainDetails }, Cmd.none )

        ExpandSpeedContent ->
            ( { model | showSpeedDetails = not model.showSpeedDetails }, Cmd.none )

        ExpandStackContent ->
            ( { model | showStackDetails = not model.showStackDetails }, Cmd.none )

        ExpandStructContent ->
            ( { model | showStructDetails = not model.showStructDetails }, Cmd.none )

        UrlChange newInput ->
            ( { model | input = newInput, websiteUrl = U.fromString newInput, isValidUrl = (checkForValidity model.websiteUrl) }, Cmd.none )

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



---- Subscriptions ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



---- Functions ----


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


createGraph : List StructureItem -> Graph String ()
createGraph items =
    let
        labels =
            createUniqueList (createListFromItems items)

        dict =
            createDict (createUniqueList (createListFromItems items))
    in
    Graph.fromNodeLabelsAndEdgePairs labels (extractMaybebVal (createLinks items dict))


createLinks : List StructureItem -> Dict String Int -> List ( Maybe Int, Maybe Int )
createLinks list dict =
    List.map (\record -> ( Dict.get record.source dict, Dict.get record.dest dict )) list


createListFromItems : List StructureItem -> List String
createListFromItems items =
    List.append (List.map (\record -> record.source) items) (List.map (\record -> record.dest) items)

checkForValidity : Maybe Url -> Bool
checkForValidity url = 
    case url of 
        Just val -> True
        Nothing -> False

empty : Html msg
empty =
    Html.text ""


renderIf : Bool -> Html msg -> Html msg
renderIf shouldRender elem =
    if shouldRender then
        elem

    else
        empty


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



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "main-section" ]
        [ div [ class "header" ]
            [ div [ class "btn-group" ]
                [ input [ class "urlInput", placeholder "http://", type_ "text", value model.input, onInput UrlChange ]
                    []
                , span [ class "searchclear", onClick ClearModel ]
                    [ text "X" ]
                ]
            ]
        , viewInfo
        , viewDomain model
        , viewSpeed model
        , viewStack model
        , viewStructure model
        , button [ class "startButton", onClick ClickCheckWebsite, disabled (not model.isValidUrl) ]
            [ text "Check" ]
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
                [ h1 []
                    [ text "Organization" ]
                , p []
                    [ text model.domainOwnershipDetails.organization ]
                ]
            ]
        , div [ class "card" ]
            [ img [ alt "Image", src "../images/map-solid.svg" ]
                []
            , div [ class "container" ]
                [ h1 []
                    [ text "State" ]
                , p []
                    [ text model.domainOwnershipDetails.state ]
                ]
            ]
        , div [ class "card" ]
            [ img [ alt "Image", src "../images/globe-solid.svg" ]
                []
            , div [ class "container" ]
                [ h1 []
                    [ text "Country" ]
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
                [ h1 []
                    [ text "Server Responstime" ]
                , p []
                    [ text (String.concat [ R.round 2 (model.speedDetails.serverResponseTime / 1000), " Sekunden" ]) ]
                ]
            ]
        , div [ class "card" ]
            [ img [ alt "Image", src "../images/paint-brush-solid.svg" ]
                []
            , div [ class "container" ]
                [ h1 []
                    [ text "Time untill first content is drawn" ]
                , p []
                    [ text (String.concat [ R.round 2 (model.speedDetails.firstContentfulPaint / 1000), " Sekunden" ]) ]
                ]
            ]
        , div [ class "card" ]
            [ img [ alt "Image", src "../images/mouse-solid.svg" ]
                []
            , div [ class "container" ]
                [ h1 []
                    [ text "Time untill site is interactive" ]
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


viewInfo : Html Msg
viewInfo =
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
    let
        totalLinks =
            String.fromInt (List.length model.structDetails.items)

        brokenLinks =
            String.fromInt (List.length (model.structDetails.items |> List.filter (\record -> record.status /= 200)))
    in
    div [ class "struct-content-section" ]
        [ initGraph (createGraph model.structDetails.items) |> viewGraph
        , div [ class "card" ]
            [ div [ class "container" ]
                [ h1 []
                    [ text (String.concat [ "I checked ", totalLinks, " links and found ", brokenLinks, " links that were broken" ])
                    ]
                ]
            ]
        ]



---- PROGRAM ----


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
