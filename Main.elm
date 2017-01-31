module Main exposing (..)

--import TimeTravel.Html exposing (beginnerProgram)
import Html exposing (beginnerProgram)
import View exposing (view)
import Model exposing (..)
import Translate exposing (translate)
import Ast


fbstr : String
fbstr = """
main : Html.Html msg
main = Html.ul [] (List.range 1 100 |> List.map (\\n ->
  Html.li [] [ Html.text (Main.fizzbuzz n) ]))

fizzbuzz : Int -> String
fizzbuzz n = (
    if n % 3 == 0 && n % 5 == 0 then "fizzbuzz"
    else if n % 3 == 0 then "fizz"
    else if n % 5 == 0 then "buzz"
    else Basics.toString n)

"""

a = """
fizzbuzz : Int -> String
fizzbuzz n =
    case ( n % 3 == 0, n % 5 == 0) of
      ( True, True ) -> "fizzbuzz"
      ( True, False ) -> "fizz"
      ( False, True ) -> "buzz"
      ( False, False ) -> Basics.toString n

"""


init =
    { src = fbstr
    , ast = translate (Ast.parse fbstr)
    , focus = ()
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

--main : Program Never Model Msg
main = beginnerProgram { model = init, update = update, view = view }
