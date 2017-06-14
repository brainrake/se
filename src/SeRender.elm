module SeRender exposing (..)

import Html exposing (Html)
import SeColor exposing (..)
--import Basics.Extra exposing ((=>))
--import Style exposing (..)


css_ : String
css_ = """
html {
    background-color: #111;
    color: """ ++ base07 ++ """;
    font-family: sans;
}

span {
  display: inline-block;
  padding: 1px;
}
"""
