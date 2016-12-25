module Ecosystem exposing (..)

type alias Package =
    { name : String
    }


type alias Module =
    { name : String
    , exports : List Binding
    }


type alias Binding =
    { name : String
    , type_ : String
    , definition : Code
    }

type QualifiedName
    = QualifiedName (List String) String



type alias Code
    = String
