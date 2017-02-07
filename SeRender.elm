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

--css_ : String
--css_ = """
--html {
--    background-color: #111;
--    color: """ ++ base07 ++ """;
--    font-family: sans;
--}

--span {
--  display: inline-block;
--  padding: 1px;
--}
--"""


--css_rules : List Rule
--css_rules =
--    [ Rule (Type "html")
--        [ "background-color" => "#111"
--        , "color" => base07
--        , "font-family" => "sans"
--        ]
--    , Rule (Type "span")
--        [ "display" => "inline-block"
--        , "padding" => "1px"
--        ]


--div_with_style : Html msg -> List ( String,  String ) -> Html msg
--div_with_style html =
--    Html.div []
--        [ Css.style [] [ Css.stylesheet [] css_rules ]
--        , Html.div [] [ html ]
--        ]

--a = 1
