module Types exposing (..)

import Http exposing (Header)
import Url as U exposing (Url)


type alias Model =
    { input : String
    , websiteUrl : Maybe Url
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


type Msg
    = ClickCheckWebsite
    | ClearModel
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
