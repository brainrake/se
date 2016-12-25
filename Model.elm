module Model exposing (..)

import Lang exposing (..)

type Msg =
      Nop
    | ToggleBorders
    | ToggleQualifiers

type alias Model =
  { module_ : Module
  , show_borders : Bool
  , show_qualifiers : Bool
  }
