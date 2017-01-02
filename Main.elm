module Main exposing (..)

import Html
import List
import View exposing (view)
import Lang exposing (..)
import Model exposing (..)


fizzbuzz0 : Int -> String
fizzbuzz0 n =
    let str = (if n % 3 == 0 then "fizz" else "") ++
              (if n % 5 == 0 then "buzz" else "")
    in if String.isEmpty str then toString n else str


fizzbuzz : Int -> String
fizzbuzz n =
    if n % 3 == 0 && n % 5 == 0 then "fizzbuzz"
    else if n % 3 == 0 then "fizz"
    else if n % 5 == 0 then "buzz"
    else n |> toString


{-

fizzbuzz n
  if n is divisible by 3 and n is divisible by 5 then "fizzbuzz"
  if n is divisible by 3 then "fizz"
  if n is divisible by 5 then "buzz"
  else n as String

-}


main_ : Html.Html msg
main_ =
    Html.ul [] (List.range 1 100 |> List.map (\n ->
        Html.li [] [ Html.text (fizzbuzz n) ]))



module__ : Module
module__ =
    [ ("fizzbuzz", Just (TArrow (TName (["Basics"], "Int")) (TName (["Basics"], "String"))), Lam "n"
        (Let [("str", Nothing,
          (Apply
            (Apply
              (Var (["Basics"], "++"))
              (If
                (Apply
                  (Apply
                    (Var (["Basics"], "=="))
                    (Apply
                      (Apply (Var (["Basics"], "%")) (Var ([], "n")))
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
                  (Var (["Basics"], "=="))
                  (Apply
                    (Apply (Var (["Basics"], "%")) (Var ([], "n")))
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
            (Apply (Var (["String"], "isEmpty")) (Var ([], "str")))
            (Apply (Var (["Basics"], "toString")) (Var ([], "n")))
            (Var ([], "str"))
          )
        )
      )
    , ("main" , Just (TApply (TName (["Html"], "Html")) (TVar "msg")),
        (Apply
          (Apply (Var (["Html"], "div")) (Var (["List"], "[]")))
          (Apply
            (Apply
              (Var (["Basics"], "|>"))
              (Apply
                (Apply (Var (["List"], "range")) (Lit (Num 1)))
                (Lit (Num 100))
              )
            )
            (Apply
              (Var (["List"], "map"))
              (Lam "n"
                (Apply
                  (Apply (Var (["Html"], "div")) (Var (["List"], "[]")))
                  (Apply
                    (Apply
                      (Var (["List"], "::"))
                      (Apply
                        (Var (["Html"], "text"))
                        (Apply (Var (["Main"], "fizzbuzz")) (Var ([], "n")))
                      )
                    )
                    (Var (["List"], "[]"))
                  )
                )
              )
            )
          )
        )
      )
    ]

module_ : Module
module_ =
    [ ("fizzbuzz", Just (TArrow (TName (["Basics"], "Int")) (TName (["Basics"], "String"))), Lam "n"
        (If
          (Apply
            (Apply
              (Var (["Basics"], "&&"))
              (Apply
                (Apply
                  (Var (["Basics"], "=="))
                  (Apply
                    (Apply (Var (["Basics"], "%")) (Var ([], "n")))
                    (Lit (Num 3))
                  )
                )
                (Lit (Num 0))
              )
            )
            (Apply
              (Apply
                (Var (["Basics"], "=="))
                (Apply
                  (Apply (Var (["Basics"], "%")) (Var ([], "n")))
                  (Lit (Num 5))
                )
              )
              (Lit (Num 0))
            )
          )
          (Lit (Str "fizzbuzz"))
          (If
            (Apply
              (Apply
                (Var (["Basics"], "=="))
                (Apply
                  (Apply (Var (["Basics"], "%")) (Var ([], "n")))
                  (Lit (Num 3))
                )
              )
              (Lit (Num 0))
            )
            (Lit (Str "fizz"))
            (If
              (Apply
                (Apply
                  (Var (["Basics"], "=="))
                  (Apply
                    (Apply (Var (["Basics"], "%")) (Var ([], "n")))
                    (Lit (Num 5))
                  )
                )
                (Lit (Num 0))
              )
              (Lit (Str "buzz"))
              (Apply (Var (["Basics"], "toString")) (Var ([], "n")))
            )
          )
        )
      )
    , ("main" , Just (TApply (TName (["Html"], "Html")) (TVar "msg")),
        (Apply
          (Apply (Var (["Html"], "ul")) (Var (["List"], "[]")))
          (Apply
            (Apply
              (Var (["Basics"], "|>"))
              (Apply
                (Apply (Var (["List"], "range")) (Lit (Num 1)))
                (Lit (Num 100))
              )
            )
            (Apply
              (Var (["List"], "map"))
              (Lam "n"
                (Apply
                  (Apply (Var (["Html"], "li")) (Var (["List"], "[]")))
                  (Apply
                    (Apply
                      (Var (["List"], "::"))
                      (Apply
                        (Var (["Html"], "text"))
                        (Apply (Var (["Main"], "fizzbuzz")) (Var ([], "n")))
                      )
                    )
                    (Var (["List"], "[]"))
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

init = { module_ = module_
    , show_borders = True
    , show_qualifiers = True }

main : Program Never Model Msg
main = Html.beginnerProgram { model = init, update = update, view = view }
