module Main exposing (..)

----Draggable will be used to check the layout of current and furure elements

import Browser
import Browser.Events as Events
import Draggable
import Html exposing (Html, button, div, h1, h2, input, p, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Http.Xml
import Json.Decode as D
import Url as U
import Xml.Decode as X



---- MODEL ----


type alias Model =
    { websiteUrl : String
    , position : ( Int, Int )
    , drag : Draggable.State String
    , speedDetails : SpeedDetails
    , domainOwnershipDetails : DomainOwnershipDetails
    }


initialModel : Model
initialModel =
    { websiteUrl = ""
    , position = ( 0, 0 )
    , drag = Draggable.init
    , speedDetails = SpeedDetails "0" "0"
    , domainOwnershipDetails = DomainOwnershipDetails "0" "0" "0"
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickCheckWebsite ->
            ( model, Cmd.batch [ fetchFromGooglePageSpeedTest, fetchFromWhoIsXML ] )

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


checkWebsite : String -> Cmd Msg
checkWebsite websiteUrl =
    Cmd.none


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
    div [ class "container" ]
        [ input [ placeholder "Name of the Site", class "text-center" ] []
        , p [ class "text-center" ]
            [ button [ class "btn btn-success", onClick ClickCheckWebsite ] [ text "Check" ]
            ]
        , div [ class "output" ]
            [ renderSpeedDetails model.speedDetails ]
        , div [ class "output" ]
            [ renderDomainDetails model.domainOwnershipDetails ]
        ]



---- PROGRAM ----


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
