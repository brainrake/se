module Main exposing (..)

import Html
import List
import View exposing (view)
import Lang exposing (..)
import Model exposing (..)
import Result exposing (Result)
import Translate exposing (translate)
import Ast


fbstr = """
fizzbuzz : Int -> String
fizzbuzz n = (
    if n % 3 == 0 && n % 5 == 0 then "fizzbuzz"
    else if n % 3 == 0 then "fizz"
    else if n % 5 == 0 then "buzz"
    else n |> Basics.toString)

main : Html.Html msg
main = Html.ul [] (List.range 1 100 |> List.map (\\n ->
  Html.li [] [ Html.text (Main.fizzbuzz n) ]))
"""


update : Msg -> Model -> Model
update msg model = case msg of
  Nop -> model
  ChangeSrc src -> { model | src = src, ast = translate (Ast.parse src)}
  ToggleBorders -> { model | show_borders = not model.show_borders }
  ToggleQualifiers -> { model | show_qualifiers = not model.show_qualifiers }

init =
  { show_borders = True
  , show_qualifiers = True
  , src = fbstr
  , ast = translate (Ast.parse fbstr)}

main : Program Never Model Msg
main = Html.beginnerProgram { model = init, update = update, view = view }
