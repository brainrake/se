fizzbuzz0 : Int -> String
fizzbuzz0 n =
    let str = (if n % 3 == 0 then "fizz" else "") ++
              (if n % 5 == 0 then "buzz" else "")
    in if String.isEmpty str then toString n else str




fizzbuzz_ n =
    if n % 3 == 0 && n % 5 == 0 then "fizzbuzz"
    else if n % 3 == 0 then "fizz"
    else if n % 5 == 0 then "buzz"
    else toString n


main_ : Html.Html msg
main_ =
    Html.ul [] (List.range 1 100 |> List.map (\n ->
        Html.li [] [ Html.text (fizzbuzz_ n) ]))



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
                      (Lit (Int 3))
                    )
                  )
                  (Lit (Int 0))
                )
                (Lit (String "fizz"))
                (Lit (String ""))
              )
            )
            (If
              (Apply
                (Apply
                  (Var (["Basics"], "=="))
                  (Apply
                    (Apply (Var (["Basics"], "%")) (Var ([], "n")))
                    (Lit (Int 5))
                  )
                )
                (Lit (Int 0))
              )
              (Lit (String "buzz"))
              (Lit (String ""))
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
                (Apply (Var (["List"], "range")) (Lit (Int 1)))
                (Lit (Int 100))
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
                    (Lit (Int 3))
                  )
                )
                (Lit (Int 0))
              )
            )
            (Apply
              (Apply
                (Var (["Basics"], "=="))
                (Apply
                  (Apply (Var (["Basics"], "%")) (Var ([], "n")))
                  (Lit (Int 5))
                )
              )
              (Lit (Int 0))
            )
          )
          (Lit (String "fizzbuzz"))
          (If
            (Apply
              (Apply
                (Var (["Basics"], "=="))
                (Apply
                  (Apply (Var (["Basics"], "%")) (Var ([], "n")))
                  (Lit (Int 3))
                )
              )
              (Lit (Int 0))
            )
            (Lit (String "fizz"))
            (If
              (Apply
                (Apply
                  (Var (["Basics"], "=="))
                  (Apply
                    (Apply (Var (["Basics"], "%")) (Var ([], "n")))
                    (Lit (Int 5))
                  )
                )
                (Lit (Int 0))
              )
              (Lit (String "buzz"))
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
                (Apply (Var (["List"], "range")) (Lit (Int 1)))
                (Lit (Int 100))
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

