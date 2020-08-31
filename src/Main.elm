module Main exposing (..)

import Browser
import Browser.Events as Events
----Draggable will be used to check the layout of current and furure elements 
import Draggable 
import Html exposing (Html, button, div, h1, h2, p, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)



---- MODEL ----


type alias Model =
    { websiteUrl : String
    , position : ( Int, Int )
    , drag : Draggable.State String
    }


initialModel : Model
initialModel =
    { websiteUrl = ""
    , position = ( 0, 0 )
    , drag = Draggable.init
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )



---- UPDATE ----


type Msg
    = ClickCheckWebsite
    | OnDragBy Draggable.Delta
    | DragMsg (Draggable.Msg String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickCheckWebsite ->
            ( model, checkWebsite model.websiteUrl )

        OnDragBy ( dx, dy ) ->
            let
                ( x, y ) =
                    model.position
            in
            ( { model | position = ( round (toFloat x + dx), round (toFloat y + dy) ) }, Cmd.none )
        
        DragMsg dragMsg ->
            Draggable.update dragConfig dragMsg model



---- Custom Types ----
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


---- Constants ----
---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ input [ placeholder "Name of the Site", class "text-center" ] []
        , p [ class "text-center" ]
            [ button [ class "btn btn-success", onClick ClickCheckWebsite ] [ text "Check" ]
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
