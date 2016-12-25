module View exposing (view)

import Html exposing (Html, div, span, text, pre, button)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import String exposing (join)
import List exposing (map)
import Char exposing (isUpper, isLower)
import Maybe.Extra exposing ((?))
import Lang exposing (..)
import Model exposing (..)

view : Model -> Html Msg
view model = div []
  [ Html.node "style" [] [ text (css model) ]
  , div [ class "config" ] [ view_config model ]
  , div [ class "code" ] [ view_module model.module_ ] ]

view_config : Model -> Html Msg
view_config model = div []
  [ button [ onClick ToggleBorders ] [ text "borders" ]
  , button [ onClick ToggleQualifiers ] [ text "qualifiers" ] ]


view_module : Module -> Html Msg
view_module bindings = span []  (bindings |> map view_binding)

view_binding : Binding -> Html Msg
view_binding (name, mtyp, exp) = span [ class "binding" ] <| case mtyp of
  Just typ ->
    [ div [ class "binding_typ"]
      [ span [ class "name" ] [ text name ]
      , keyword ":"
      , view_typ typ
      , keyword "=" ]
    , view_exp exp ]
  Nothing ->
    [ span [ class "name" ] [ text name ]
    , keyword "="
    , view_exp exp ]

view_typ : Typ -> Html Msg
view_typ typ = case typ of
  TName qname -> span [ class "typ name" ] [ view_qname qname ]
  TVar var -> span [ class "typ var" ] [ text var ]
  TApply t1 t2 -> span [ class "typ apply" ]
    [ view_typ t1
    , span [ class "paren" ] [ text " (" ]
    , view_typ t2
    , span [ class "paren" ] [ text ") "] ]
  TArrow t1 t2 -> span [ class "typ arrow" ]
    [ view_typ t1
    , keyword "->"
    , view_typ t2 ]


view_qname : QualifiedName -> Html Msg
view_qname (qs, name) = span [ class "qualifiedname" ] <|
  (qs |> map (\q -> span [ class "qualifier" ] [ text (q ++ ".") ]))
  ++ [ span [ class "unqualifiedname" ] [ text name ] ]

view_exp : Exp -> Html Msg
view_exp exp = case exp of
  Let bindings exp -> span [ class "exp let" ]
    [ keyword "let"
    , span [] (bindings |> map view_binding)
    , keyword "in"
    , view_exp exp ]
  Lam name exp -> span [ class "exp lam" ]
    [ keyword "λ"
    , span [ class "name" ] [ text name ]
    , span [ class "keyword" ] [text " → " ]
    , view_exp exp ]
  Apply f x ->
    let view_apply = span [ class "exp apply" ] <|
      [ span [ class "paren" ] [ text "(" ]
      , view_exp f
      , keyword " "
      , view_exp x
      , span [ class "paren" ] [ text ")" ] ]
    in case f of
      (Apply (Var (qs, fn)) y) ->
        if (String.uncons fn |> Maybe.map (Tuple.first >> (\c -> isUpper c || isLower c))) ? True
        then view_apply
        else span [ class "exp apply op" ]
          [ span [ class "paren" ] [ text "(" ]
          , view_exp y
          , text " "
          , view_exp (Var (qs, fn))
          , text " "
          , view_exp x
          , span [ class "paren" ] [ text ")" ] ]
      _ -> view_apply
  Var qname -> span [ class "exp var" ] [ view_qname qname ]
  If cond then_ else_ -> span [ class "exp if" ]
    [ keyword "if"
    , view_exp cond
    , keyword "then"
    , view_exp then_
    , keyword "else"
    , view_exp else_ ]
  Case cases -> span [ class "exp case" ] (cases |> map (\(pat, exp) -> span [] [ view_pattern pat, text " => ", view_exp exp ] ))
  Lit lit -> span [ class "exp lit" ] [ view_literal lit ]


view_literal : Literal -> Html Msg
view_literal lit = case lit of
  Str lit -> text ("\"" ++ lit ++ "\"")
  Num lit -> text (toString lit)


view_pattern : Pattern -> Html Msg
view_pattern pattern = span [ class "pattern" ] [ text <| case pattern of
  PApply ps -> join "\n" (ps |> map toString)
  PName n -> n ]

keyword : String -> Html a
keyword str = span [ class "keyword" ] [ text (" " ++ str ++ " ") ]



css : Model -> String
css model = """
* {
  font-family: sans;
}

.code {
  #width: 600px;
}

span {
  display: inline-block;
  padding: 1px;
}

.exp.var {
  color: black;
}

.exp.apply, .exp.if, .exp.apply.op, .exp.lam, .binding, .typ.apply, .typ.arrow {
  color: black;
  border-width : 0;
""" ++ (if model.show_borders then """
  border: 1px solid grey;
  padding: 2px;
  margin: 2px;
  border-radius: 4px;
""" else "") ++ """
}

.exp.if {
  color: orange;
  border-style: dashed;
  border-color: orange;
}

.exp.apply.op, .typ.arrow {
  border-color: lightgrey;
}

.typ, .typ.name {
  color: red;
  border-color: red;
}

.lit {
  color: darkcyan;
  border-color: cyan;
}

.name, .binding, .exp.lam, .typ.var{
  color: purple;
  border-color: purple;
}

.keyword {
  color: orange;
}

.paren {
  display: none;
  color: grey;
}


.exp.let {
  display: inline;
}

.qualifier {
  color: grey;
""" ++ (if model.show_qualifiers then "display:none" else "") ++ """
}

"""
