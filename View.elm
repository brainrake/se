module View exposing (view)

import Html exposing (Html, div, span, text, pre, button, table, thead, tbody, th, tr, td)
import Html.Attributes exposing (class, style, rows, cols)
import Html.Events exposing (onInput)
import Html.Events exposing (onClick)
import String exposing (join)
import List exposing (map, isEmpty, indexedMap)
import Char exposing (isUpper, isLower)
import Maybe.Extra exposing ((?))
import Lang exposing (..)
import Model exposing (..)


view : Model -> Html Msg
view model = div []
  [ Html.node "style" [] [ text (css model) ]
  --, div [] [ text model.ast ]
  , div [ class "source" ]
    [ Html.textarea [ cols 80, rows 12, onInput ChangeSrc ] [ text model.src ]
    ]
  , div [ class "config" ] [ view_config model.opts |> Html.map OptionsMsg ]
  , div [ class "module" ] [ view_module model.opts model.ast ]
  --, div [ class "module" ] [ view_module model.module_ ]
  ]


view_config : Options -> Html OptionsMsg
view_config model = div []
    [ button [ onClick <| ToggleBorders ] [ text "Boxes" ]
    , button [ onClick <| ToggleQualifiers ] [ text "Full Names" ]
    , button [ onClick ToggleInfix ] [ text "Infix Operators" ]
    ]


view_module : Options -> Module -> Html Msg
view_module opts { name, imports, bindings } =
    div [] (bindings |> map (view_binding opts FPoint))



view_binding : Options -> Focus -> Binding -> Html Msg
view_binding opts here (name, mtyp, exp) =
    let view_binding_with_typ typ =
            [ div [ class "binding_typ" ]
                [ span [ class "name" ] [ text name ]
                , keyword ":"
                , view_typ (FBindingTyp here) typ
                , keyword ""
                , span [ style [("color", "#" ++ base02)]] [ text "with " ]
                , span [ style [("color", "#" ++ base03)]] [ text "Basics, " ]
                , span [ style [("color", "#" ++ base03)]] [ text "List, " ]
                , span [ style [("color", "#" ++ base03)]] [ text "Html" ]
                ]
            , div [ class "binding_val" ] [ view_exp opts (FBindingValue here) exp ]
            ]
        view_binding_no_typ =
            [ span [ class "name" ] [ text name ]
            , keyword "="
            , view_exp opts (FBindingValue here) exp
            ]
    in div []
        [ div [ class "binding" ] <|
            case mtyp of
                Just typ -> view_binding_with_typ  typ
                Nothing -> view_binding_no_typ
        ]


view_typ : Focus -> Typ -> Html Msg
view_typ here typ = case typ of
    TName qname ->
        span [ class "typ name" ] [ view_qname (FTypName here) qname ]
    TVar var ->
        span [ class "typ var" ] [ text var ]
    TApply t1 t2 ->
        span [ class "typ apply" ]
            [ view_typ (FTypApplyFun here) t1
            , span [ class "paren" ] [ text " (" ]
            , view_typ (FTypApplyArg here) t2
            , span [ class "paren" ] [ text ") "]
            ]
    TArrow t1 t2 ->
        span [ class "typ arrow" ]
            [ view_typ (FTypArrowArg here) t1
            , keyword "→"
            , view_typ (FTypArrowResult here) t2
            ]


view_qname : Focus -> QualifiedName -> Html Msg
view_qname here (qs, name) = case qs of
  Nothing ->
    span [ class "unqualifiedname" ] [ text name ]
  Just qualifier ->
    span [ class "qualifiedname" ]
      [ span [ class "qualifier" ] [ text (qualifier ++ ".") ]
      , span [ class "unqualifiedname" ] [ text name ]
      ]


view_exp : Options -> Focus -> Exp -> Html Msg
view_exp opts here exp = case exp of
    Let bindings exp ->
        span [ class "exp let" ]
            [ keyword "let"
            , span [] (bindings |> indexedMap (\n b ->
                view_binding opts (FLetBinding n here) b))
            , keyword "in"
            , view_exp opts (FLetExp here) exp
            ]
    Lam name exp ->
        span [ class "exp lam" ]
            [ keyword "λ"
            , span [ class "name" ] [ text name ]
            , span [ class "keyword" ] [text " → " ]
            , Html.br [] []
            , view_exp opts (FLamExp here) exp
            ]
    Apply f x ->
        view_apply opts here f x
    Var qname ->
        span [ class "exp var" ] [ view_qname (FVar here) qname ]
    Case exp cases ->
        view_patmat opts here exp cases
    Lit lit ->
        span [ class "exp lit" ] [ view_literal (FLit here) lit ]
    Record _ ->
        text "record"


view_apply : Options -> Focus -> Exp -> Exp -> Html Msg
view_apply opts here f x =
    let rendered = span [ class "exp apply" ] <|
        [ span [ class "paren" ] [ text "(" ]
        , view_exp opts (FApplyFun here) f
        , keyword " "
        , view_exp opts (FApplyArg here) x
        , span [ class "paren" ] [ text ")" ]
        ]
    in case f of
        (Apply (Var (qs, fn)) y) ->
            if (String.uncons fn |> Maybe.map (Tuple.first >> (λc -> isUpper c || isLower c))) ? True
            then rendered
            else span [ class "exp apply op" ]
                [ span [ class "paren" ] [ text "(" ]
                , view_exp opts (FApplyFun (FApplyFun here)) y
                , text " "
                , view_exp opts (FApplyFun (FApplyArg here)) (Var (qs, fn))
                , text " "
                , view_exp opts (FApplyArg here) x
                , span [ class "paren" ] [ text ")" ] ]
        _ -> rendered


view_patmat : Options -> Focus -> Exp -> List ( Pattern, Exp ) -> Html Msg
view_patmat opts here exp cases =
    let view_case n (pat, exp) =
        tr []
            [ td [] [ view_pattern (FCasePattern n here) pat, text " → " ]
            , td [] [ view_exp opts (FCaseResult n here) exp ]
            ]
    in table [ class "exp case if" ] <|
        [ tr []
            [ td [] [ keyword "case" ]
            , td [] [ view_exp opts (FCaseExp here) exp ]
            ]
        ] ++ (cases |> indexedMap view_case )


view_literal : Focus -> Literal -> Html Msg
view_literal here lit =
    case lit of
        String lit -> text ("\"" ++ lit ++ "\"")
        Char lit -> text ("'" ++ toString lit ++ "'")
        Int lit -> text (toString lit)
        Float lit -> text (toString lit)


view_pattern : Focus -> Pattern -> Html Msg
view_pattern here pattern =
    span [ class "pattern" ] [ text <|
        case pattern of
            PApply ps -> join "\n" (ps |> map toString)
            PCon (q, n) -> n
            PVar n -> n
            PLit lit -> toString lit ]

keyword : String -> Html a
keyword str =
    span [ class "keyword" ] [ text (" " ++ str ++ " ") ]

{-
bg color = [("background-color", color)]
fg color = [("color", color)]
bc color = [("border-color", color)]
c color = fg color ++ bc color

-}

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
  background-color: #111;
  font-family: sans;
  """ ++ fg base07 ++ """
}

.module {
  --width: 50%;
  background-color: #000;
}

span {
  display: inline-block;
  padding: 1px;
  font-family: Sans;
  font-size: 1.0em;
  font-style: normal;
  """ ++ bg base00 ++ """
}

.module > div > div > div {
  """ ++ bg base00 ++ """
  margin-top: 5px;
  border: 0;
}

.exp.var {
  """ ++ fg base07 ++ """
}

.exp.apply, .exp.if, .exp.apply.op, .exp.lam, .binding, .typ.apply, .typ.arrow {
  border-width : 0;
""" ++ (if model.opts.show_borders then """
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
""" ++ (if model.opts.show_qualifiers then "display:none" else "") ++ """
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

textarea {
  background-color: black;
  color: white;
}

"""
