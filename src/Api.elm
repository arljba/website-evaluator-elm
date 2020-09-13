module Api exposing (fetchFromGooglePageSpeedTest,fetchFromWhoIsXML,fetchFromBuiltwith,fetchFromCrawler)

import Http exposing (Header)
import Http.Xml
import Json.Decode as D
import Types exposing (..)
import Url as U exposing (Url)
import Xml.Decode as X


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
                    { url = String.concat [ "https://api.builtwith.com/v17/api.json?KEY=3d993c48-5c46-4f64-8ba7-7cf9cace11fb&LOOKUP=", U.toString url, "&HIDEDL=no" ]
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
