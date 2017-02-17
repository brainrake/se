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
  Html.li [] [ Html.text (Main.fizzBuzz n) ]))

fizzBuzz : Int -> String
fizzBuzz n = (
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


init : Model
init =
    { init_model
    | src = fbstr
    , ast = translate (Ast.parse fbstr)
    }


update_opts : OptionsMsg -> Options -> Options
update_opts msg opts =
    case msg of
        Source x -> { opts | source = x }
        Borders x -> { opts | borders = x }
        Parens x -> { opts | parens = x }
        Qualifiers x -> { opts | qualifiers = x }
        Infix x -> { opts | infix = x }
        Snake x -> { opts | snake = x }


update : Msg -> Model -> Model
update msg model = case msg of
    Nop ->
        model
    ChangeSrc src ->
        { model | src = src, ast = translate (Ast.parse src)}
    OptionsMsg optsmsg ->
        { model | opts = update_opts optsmsg model.opts }
    ChangeCursor cursor ->
        { model | cursor = cursor }


--main : Program Never Model Msg
main = beginnerProgram { model = init, update = update, view = view }
