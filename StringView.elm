module StringView exposing (..)

import Html exposing (Html, div, span, text, pre)
import String exposing (join)
import List exposing (map)
import Lang exposing (..)
import Model exposing (..)

view_typ_s : Typ -> String
view_typ_s typ = case typ of
  TName name -> name
  TVar name -> name
  TApply t1 t2 -> view_typ_s t1 ++ " " ++ view_typ_s t2
  TArrow t1 t2 -> view_typ_s t1 ++ " -> " ++ view_typ_s t2

view_pattern_s : Pattern -> String
view_pattern_s pattern = case pattern of
  PApply ps -> join "\n" (ps |> map toString)
  PName n -> n

view_binding_s : Binding -> String
view_binding_s (name, mtyp, exp) =
  let type_annotation = case mtyp of
    Just typ -> name ++ " : " ++ view_typ_s typ ++ "\n"
    Nothing -> ""
  in type_annotation ++ name ++ " = " ++ view_exp_s exp

view_exp_s : Exp -> String
view_exp_s exp = case exp of
  Let bindings exp -> "\nlet " ++ join "\n "(bindings |> map view_binding_s) ++ "\nin " ++ view_exp_s exp
  Lam name exp -> "\\" ++ name ++ " -> " ++ view_exp_s exp
  Apply f x -> "(" ++ view_exp_s f ++ " " ++ view_exp_s x ++ ")"
  Var name -> name
  If cond then_ else_ -> "if " ++ view_exp_s cond ++ " then " ++ view_exp_s then_ ++ " else " ++ view_exp_s else_
  Case cases -> join "\n" (cases |> map (\(pat, exp) -> view_pattern_s pat ++ " => " ++ view_exp_s exp))
  Lit lit -> "\"" ++ lit ++ "\""

view_module_s : Module -> String
view_module_s bindings = join "\n" (bindings |> map view_binding_s)

view : Module -> Html Msg
view model = pre [] [ text <| view_module_s model ]
