module Model exposing (..)

import Lang exposing (..)


type Msg
    = Nop
    | OptionsMsg OptionsMsg
    | ChangeSrc String


type OptionsMsg
    = Source Bool
    | Borders Bool
    | Parens Bool
    | Qualifiers Bool
    | Infix Bool
    | Snake Bool


type alias Model =
    { ast : Module
    , src : String
    , opts : Options
    , focus : ()
    }


type alias Options =
    { source : Bool
    , borders : Bool
    , parens : Bool
    , qualifiers : Bool
    , infix : Bool
    , snake : Bool
    }
