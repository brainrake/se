module Model exposing (..)

import Lang exposing (..)

type Msg =
      Nop
    | ToggleBorders
    | ToggleQualifiers
    | ChangeSrc String

type alias Model =
  { show_borders : Bool
  , show_qualifiers : Bool
  , ast : Module
  , src : String
  }
