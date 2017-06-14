module SeColor exposing (..)

import Basics.Extra exposing ((=>))


base00 : String
base00 = "#263238"

base01 : String
base01 = "#2C393F"

base02 : String
base02 = "#37474F"

base03 : String
base03 = "#707880"

base04 : String
base04 = "#C9CCD3"

base05 : String
base05 = "#CDD3DE"

base06 : String
base06 = "#D5DBE5"

base07 : String
base07 = "#FFFFFF"

base08 : String
base08 = "#EC5F67"

base09 : String
base09 = "#EA9560"

base0A : String
base0A = "#FFCC00"

base0B : String
base0B = "#8BD649"

base0C : String
base0C = "#80CBC4"

base0D : String
base0D = "#89DDFF"

base0E : String
base0E = "#82AAFF"

base0F : String
base0F = "#EC5F67"


bg : String -> List (String, String)
bg color = [ "background-color" => color ]

fg : String -> List (String, String)
fg color = [ "color" => color ]

bc : String -> List (String, String)
bc color = [ "border-color" => color ]

c : String -> List (String, String)
c color = fg color ++ bc color
