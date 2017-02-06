module Model exposing (..)

import Lang exposing (..)


type Msg
    = Nop
    | OptionsMsg OptionsMsg
    | ChangeSrc String


type OptionsMsg
    = ShowBorders Bool
    | ShowQualifiers Bool
    | Infix Bool
    | Snake Bool


type alias Model =
    { ast : Module
    , src : String
    , opts : Options
    , focus : ()
    }


type alias Options =
    { show_borders : Bool
    , show_qualifiers : Bool
    , infix : Bool
    , snake : Bool
    }
