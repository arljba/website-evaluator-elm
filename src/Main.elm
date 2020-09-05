module Main exposing (..)

----Draggable will be used to check the layout of current and furure elements

import Browser
import Browser.Events as Events exposing (Visibility(..))
import Draggable
import Html exposing (Html, a, br, button, div, h1, h2, hr, i, input, label, li, option, p, select, small, span, text, ul)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
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
    }


initialModel : Model
initialModel =
    { websiteUrl = ""
    , position = ( 0, 0 )
    , drag = Draggable.init
    , speedDetails = SpeedDetails "0" "0"
    , domainOwnershipDetails = DomainOwnershipDetails "0" "0" "0"
    , isValid = False
    , showDomainDetails = True
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickCheckWebsite ->
            ( model, Cmd.batch [ fetchFromGooglePageSpeedTest, fetchFromWhoIsXML ] )

        ExpandDomainContent ->
            ( { model | showDomainDetails = not model.showDomainDetails }, Cmd.none )

        UrlChange newUrl ->
            ( { model | websiteUrl = newUrl, isValid = checkWebsite newUrl }, Cmd.none )

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


type alias User =
    { id : Int
    , email : String
    }


json =
    """
{
  "id" : 1,
  "email" : "arne-baumann@gmail.com"
}
"""


type alias SpeedDetails =
    { timeToInteractive : String
    , firstContentfulPaint : String
    }


type alias DomainOwnershipDetails =
    { organization : String
    , state : String
    , country : String
    }



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


testDecoder : D.Decoder User
testDecoder =
    D.map2 User
        (D.field "id" D.int)
        (D.field "email" D.string)


decoderToString : String -> Html Msg
decoderToString string =
    case string |> D.decodeString testDecoder of
        Ok user ->
            div [ class "output" ]
                [ text user.email ]

        Err err ->
            div [ class "output" ]
                [ text "Error" ]


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



--- For some reason "d.at" doent work with D.at maybe look at it later


fetchFromGooglePageSpeedTest : Cmd Msg
fetchFromGooglePageSpeedTest =
    Http.get
        { url = "https://www.googleapis.com/pagespeedonline/v5/runPagespeed?url=https://hs-flensburg.de&&key=AIzaSyBfcmkhsGWVmLlVYn0YkTk6dDZFrcbbXV4&&category=PERFORMANCE&&strategy=DESKTOP"
        , expect = Http.expectJson GotSpeed gpstDecoder
        }


fetchFromWhoIsXML : Cmd Msg
fetchFromWhoIsXML =
    Http.get
        { url = "https://www.whoisxmlapi.com/whoisserver/WhoisService?apiKey=at_XRwFO1KDNvYMqdy0QfkAGpMhB7i58&domainName=google.com"
        , expect = Http.Xml.expectXml GotDomain wixDecoder
        }


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
            ]
        , div [ class "dashbord dashbord-domain" ]
            [ div [ class "detail-section" ]
                [ div [ class "general-info" ]
                    [ h1 []
                        [ text "Domain" ]
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
        , div [ class "dashbord dashbord-speed" ]
            [ div [ class "detail-section" ]
                [ div [ class "general-info" ]
                    [ h1 []
                        [ text "Speed" ]
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
        , div [ class "dashbord dashbord-stack" ]
            [ div [ class "detail-section" ]
                [ div [ class "general-info" ]
                    [ h1 []
                        [ text "Stack" ]
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
        ]


viewExpandDomain : Model -> Html Msg
viewExpandDomain model =
    div [ class "domain-content-section" ]
        [ p []
            [ text "Organization: " ]
        , p []
            [ text "State: " ]
        , p []
            [ text "Country: " ]
        ]



---- PROGRAM ----


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
