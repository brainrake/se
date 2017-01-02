module View exposing (view)

import Html exposing (Html, div, span, text, pre, button, table, thead, tbody, th, tr, td)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import String exposing (join)
import List exposing (map, isEmpty)
import Char exposing (isUpper, isLower)
import Maybe.Extra exposing ((?))
import Lang exposing (..)
import Model exposing (..)

view : Model -> Html Msg
view model = div []
  [ Html.node "style" [] [ text (css model) ]
  , div [ class "config" ] [ view_config model ]
  , div [ class "module" ] [ view_module model.module_ ] ]


view_config : Model -> Html Msg
view_config model = div []
  [ button [ onClick ToggleBorders ] [ text "Boxes" ]
  , button [ onClick ToggleQualifiers ] [ text "Full Names" ] ]


view_module : Module -> Html Msg
view_module bindings = div [] (bindings |> map view_binding)

view_binding : Binding -> Html Msg
view_binding (name, mtyp, exp) = div []
  [ span [ class "binding" ] <| case mtyp of
      Just typ ->
        [ div [ class "binding_typ" ]
          [ span [ class "name" ] [ text name ]
          , keyword ":"
          , view_typ typ
          , keyword "" ]
        , div [ class "binding_val" ] [ view_exp exp ]
        ]
      Nothing ->
        [ span [ class "name" ] [ text name ]
        , keyword "="
        , view_exp exp
        ] ]
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
    , keyword "→"
    , view_typ t2 ]


view_qname : QualifiedName -> Html Msg
view_qname (qs, name) =
  if isEmpty qs
  then span [ class "unqualifiedname" ] [ text name ]
  else
    span [ class "qualifiedname" ] <|
    (qs |> map (λq -> span [ class "qualifier" ] [ text (q ++ ".") ]))
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
    , Html.br [] []
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
        if (String.uncons fn |> Maybe.map (Tuple.first >> (λc -> isUpper c || isLower c))) ? True
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
    [ tr []
      [ td [] [ keyword "if" ]
      , td [] [ view_exp cond ] ]
    , tr []
      [ td [] [ keyword "then" ]
      , td [] [ view_exp then_ ] ]
    , tr []
      [ td [] [ keyword "else" ]
      , td [] [ view_exp else_ ] ] ]
  Case cases -> span [ class "exp case" ] (cases |> map (λ(pat, exp) -> span [] [ view_pattern pat, text " => ", view_exp exp ] ))
  Lit lit -> span [ class "exp lit" ] [ view_literal lit ]
  Dict _ -> text "dict"


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

base00 = "263238"
base01 = "2C393F"
base02 = "37474F"
base03 = "707880"
base04 = "C9CCD3"
base05 = "CDD3DE"
base06 = "D5DBE5"
base07 = "FFFFFF"
base08 = "EC5F67"
base09 = "EA9560"
base0A = "FFCC00"
base0B = "8BD649"
base0C = "80CBC4"
base0D = "89DDFF"
base0E = "82AAFF"
base0F = "EC5F67"


bg color = "background-color: #" ++ color ++ ";"
fg color = "color: #" ++ color ++ ";"
bc color = "border-color: #" ++ color ++ ";"
c color = fg color ++ bc color

css : Model -> String
css model = """
html {
  font-family: sans;
  """ ++ bg base00 ++ """
  """ ++ fg base07 ++ """
}

.module {
  #width: 600px;
}

span {
  display: inline-block;
  padding: 1px;
  font-family: Sans;
  font-size: 1.0em;
  font-style: normal;
  """ ++ bg base00 ++ """

}

div {
}

.exp.var {
  """ ++ fg base07 ++ """
}

.exp.apply, .exp.if, .exp.apply.op, .exp.lam, .binding, .typ.apply, .typ.arrow {
  border-width : 0;
""" ++ (if model.show_borders then """
  border-width: 1px;
  border-style: solid;
  padding: 2px;
  margin: 2px;
  border-radius: 4px;
""" else "") ++ """
}

.binding {
  padding: 0;
}

.binding_typ {
}

.binding_val {
  margin: 0;
}

.exp.if {
  border-width: 0px;
  margin: 0;
  padding: 0;
  """ ++ c base0A ++ """
  border-style: dashed;
  border-collapse: collapse;
}

.exp.if > tr > td {
  """ ++ c base0A ++ """
  border-style: dashed;
  border-width: 1px;
  border-collapse: collapse;
  padding: 1px;
}

.exp.if > tr:hover > td:first-child, .exp.if > tr:hover > td:first-child > span {
  background-color: transparent;
}

.exp.if > tr > td > * {
  margin: 2px;
}

.exp.apply {
  """ ++ bc base03 ++ """
}

.exp.apply.op {
  """ ++ bc base04 ++ """
}

.typ, .typ.name {
  """ ++ c base0B ++ """
}

.lit {
  """ ++ c base06 ++ """
  """ ++ bg base02 ++ """
  border-radius: 4px;
}

.name, .binding, .exp.lam, .typ.var{
  """ ++ c base0E ++ """
}

.exp.lam > .name, .typ.var {
  font-family: Serif;
  font-style: italic;
  font-size: 1.2em;
}

.keyword {
  """ ++ c base0A ++ """
}

.paren {
  display: none;
  color: grey;
}


.qualifier {
  color: grey;
""" ++ (if model.show_qualifiers then "display:none" else "") ++ """
}

.exp.var > .qualifiedname > .unqualifiedname {
  """ ++ c base07 ++ """
}

.exp.var > .unqualifiedname, .typ.var {
  """ ++ c base0D ++ """
  font-family: Serif;
  font-style: italic;
  font-size: 1.2em;
}

td {
  vertical-align: top;
  padding: 1px;
  font-family: Sans;
  font-size: 1.0em;
  font-style: normal;
  """ ++ bg base00 ++ """
}

span:hover, td:hover {
  background-color: #354 !important;
}

span.name:hover, span.exp.lit:hover, span.exp.var:hover, span.qualifiedname:hover, span.unqualifiedname:hover {
  background-color: #687 !important;
}


"""
