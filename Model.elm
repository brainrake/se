module Model exposing (..)

import Lang exposing (..)


type Msg
    = Nop
    | OptionsMsg OptionsMsg
    | ChangeSrc String
    | ChangeCursor (List Focus)


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
    , cursor : List Focus
    }

init_model : Model
init_model =
    { ast = init_module
    , src = ""
    , opts = init_opts
    , cursor = []
    }


type alias Options =
    { source : Bool
    , borders : Bool
    , parens : Bool
    , qualifiers : Bool
    , infix : Bool
    , snake : Bool
    }

init_opts : Options
init_opts =
    { source = False
    , borders = False
    , parens = False
    , qualifiers = False
    , infix = True
    , snake = True
    }
