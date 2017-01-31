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


init =
    { src = fbstr
    , ast = translate (Ast.parse fbstr)
    , opts =
        { show_borders = True
        , show_qualifiers = True
        , infix = True
        }
    }


update_opts : OptionsMsg -> Options -> Options
update_opts msg opts =
    case msg of
        ToggleBorders -> { opts | show_borders = not opts.show_borders }
        ToggleQualifiers -> { opts | show_qualifiers = not opts.show_qualifiers }
        ToggleInfix -> { opts | infix = not opts.infix }


update : Msg -> Model -> Model
update msg model = case msg of
    Nop ->
        model
    ChangeSrc src ->
        { model | src = src, ast = translate (Ast.parse src)}
    OptionsMsg optsmsg ->
        { model | opts = update_opts optsmsg model.opts }

main : Program Never Model Msg
main = Html.beginnerProgram { model = init, update = update, view = view }
