module Model exposing (..)

import Keyboard.Extra
import Lang exposing (..)


type Msg
    = Nop
    | OptionsMsg OptionsMsg
    | ChangeSrc String
    | ChangeCursor (List Focus)
    | ChangeCaret (List Focus)
    | KeyPress Keyboard.Extra.Key


type OptionsMsg
    = Source Bool
    | Borders Bool
    | Parens Bool
    | Qualifiers Bool
    | Infix Bool
    | Snake Bool
    | Direction Bool


type alias Model =
    { ast : Module
    , src : String
    , opts : Options
    , cursor : List Focus
    , caret : List Focus
    , keys_pressed : List Keyboard.Extra.Key
    }


init_model : Model
init_model =
    { ast = init_module
    , src = ""
    , opts = init_opts
    , cursor = []
    , caret = []
    , keys_pressed = []
    }


type alias Options =
    { source : Bool
    , borders : Bool
    , parens : Bool
    , qualifiers : Bool
    , infix : Bool
    , snake : Bool
    , direction : Bool
    }


init_opts : Options
init_opts =
    { source = False
    , borders = False
    , parens = False
    , qualifiers = False
    , infix = False
    , snake = True
    , direction = False
    }
