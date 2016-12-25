module Main exposing (..)

import Html
import List
import View exposing (view)
import Lang exposing (..)
import Model exposing (..)


fizzbuzz : Int -> String
fizzbuzz n =
    let str = (if n % 3 == 0 then "fizz" else "") ++
              (if n % 5 == 0 then "buzz" else "")
    in if String.isEmpty str then toString n else str



main_ : Html.Html msg
main_ =
    Html.div [] (List.range 1 100 |> List.map (\n ->
        Html.div [] [ Html.text (fizzbuzz n) ]))



module_ : Module
module_ =
    [ ("fizzbuzz", Just (TArrow (TName ([], "Int")) (TName ([], "String"))), Lam "n"
        (Let [("str", Nothing,
          (Apply
            (Apply
              (Var ([], "++"))
              (If
                (Apply
                  (Apply
                    (Var ([], "=="))
                    (Apply
                      (Apply (Var ([], "%")) (Var ([], "n")))
                      (Lit (Num 3))
                    )
                  )
                  (Lit (Num 0))
                )
                (Lit (Str "fizz"))
                (Lit (Str ""))
              )
            )
            (If
              (Apply
                (Apply
                  (Var ([], "=="))
                  (Apply
                    (Apply (Var ([], "%")) (Var ([], "n")))
                    (Lit (Num 5))
                  )
                )
                (Lit (Num 0))
              )
              (Lit (Str "buzz"))
              (Lit (Str ""))
            )
          ))]
          (If
            (Apply (Var ([ "String" ], "isEmpty")) (Var ([], "str")))
            (Apply (Var ([], "toString")) (Var ([], "n")))
            (Var ([], "str"))
          )
        )
      )
    , ("main" , Just (TApply (TName ([ "Html" ], "Html")) (TVar "msg")),
        (Apply
          (Apply (Var (["Html"], "div")) (Var ([], "[]")))
          (Apply
            (Apply
              (Var ([], "|>"))
              (Apply
                (Apply (Var (["List"], "range")) (Lit (Num 1)))
                (Lit (Num 100))
              )
            )
            (Apply
              (Var (["List"], "map"))
              (Lam "n"
                (Apply
                  (Apply (Var (["Html"], "div")) (Var ([], "[]")))
                  (Apply
                    (Apply
                      (Var ([], "::"))
                      (Apply
                        (Var (["Html"], "text"))
                        (Apply (Var ([], "fizzbuzz")) (Var ([], "n")))
                      )
                    )
                    (Var ([], "[]"))
                  )
                )
              )
            )
          )
        )
      )
    ]


update : Msg -> Model -> Model
update msg model = case msg of
  Nop -> model
  ToggleBorders -> { model | show_borders = not model.show_borders }
  ToggleQualifiers -> { model | show_qualifiers = not model.show_qualifiers }

main : Program Never Model Msg
main = Html.beginnerProgram
  { model =
    { module_ = module_
    , show_borders = True
    , show_qualifiers = True }
  , view = view
  , update = update }
