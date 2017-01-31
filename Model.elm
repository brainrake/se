module Model exposing (..)

import Lang exposing (..)


type Msg
    = Nop
    | OptionsMsg OptionsMsg
    | ChangeSrc String


type OptionsMsg
    = ToggleQualifiers
    | ToggleInfix
    | ToggleBorders


type alias Model =
    { ast : Module
    , src : String
    , opts : Options
    }


type alias Options =
    { show_borders : Bool
    , show_qualifiers : Bool
    , infix : Bool
    }

