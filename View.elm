module View exposing (view)

import Html exposing (Html, div, span, text, pre, button, table, thead, tbody, th, tr, td, input, label)
import Html.Attributes exposing (class, style, rows, cols, type_, checked)
import Html.Events exposing (onInput)
import Html.Events exposing (onClick)
import String exposing (join)
import List exposing (map, isEmpty, indexedMap)
import Char exposing (isUpper, isLower)
import Maybe.Extra exposing ((?))
import Lang exposing (..)
import Model exposing (..)
import SeColor exposing (..)


view_source : String -> Html Msg
view_source src =
    Html.textarea
        [ cols 80
        , rows 12
        , style ( bg "black" ++ c "white" )
        , onInput ChangeSrc
        ]
        [ text src ]


css_ : String
css_ = """
html {
    background-color: #111;
    color: """ ++ base07 ++ """;
    font-family: sans;
}
"""

view : Model -> Html Msg
view model = div []
  [ Html.node "style" [] [ text (css model) ]
  , Html.node "style" [] [ text css_ ]
  --""" ++ to_css (fg base07) ++ """
  --, div [] [ text model.ast ]
  , div [ class "source" ] [ view_source model.src ]
  , div [ class "config" ] [ view_config model.opts |> Html.map OptionsMsg ]
  , div [ class "module" ] [ view_module model.opts model.ast ]
  --, div [ class "module" ] [ view_module model.module_ ]
  ]



checkbox : String -> (Bool -> msg) -> Bool -> Html msg
checkbox name msg val =
  label
    [ style [("padding", "20px")] ]
    [ input [ type_ "checkbox", onClick <| msg (not val), checked val ] []
    , text name
    ]


view_config : Options -> Html OptionsMsg
view_config opts =
    div []
        [ checkbox "Borders" ShowBorders opts.show_borders
        , checkbox  "FullNames" ShowQualifiers opts.show_qualifiers
        , checkbox "Infix Operators" Infix opts.infix
        , checkbox "snake-case" Snake opts.snake
        ]


view_module : Options -> Module -> Html Msg
view_module opts { name, imports, bindings } =
    div [] (bindings |> map (view_binding opts FPoint))


view_binding : Options -> Focus -> Binding -> Html Msg
view_binding opts here (name, mtyp, exp) =
    let view_binding_with_typ typ =
            [ div [ class "binding_typ" ]
                    [ span [ class "name" ] [ text (snake opts name) ]
                , keyword ":"
                , view_typ opts (FBindingTyp here) typ
                , keyword ""
                , span [ style [("color", "#" ++ base02)]] [ text "with " ]
                , span [ style [("color", "#" ++ base03)]] [ text "Basics, " ]
                , span [ style [("color", "#" ++ base03)]] [ text "List, " ]
                , span [ style [("color", "#" ++ base03)]] [ text "Html" ]
                ]
            , div [ class "binding_val" ] [ view_exp opts (FBindingValue here) exp ]
            ]
        view_binding_no_typ =
            [ span [ class "name" ] [ text (snake opts name) ]
            , keyword "="
            , view_exp opts (FBindingValue here) exp
            ]
    in div []
        [ div [ class "binding" ] <|
            case mtyp of
                Just typ -> view_binding_with_typ  typ
                Nothing -> view_binding_no_typ
        ]


view_typ : Options -> Focus -> Typ -> Html Msg
view_typ opts here typ = case typ of
    TName qname ->
        span [ class "typ name" ] [ view_qname opts (FTypName here) qname ]
    TVar var ->
        span [ class "typ var" ] [ text (snake opts var) ]
    TApply t1 t2 ->
        span [ class "typ apply" ]
            [ view_typ opts (FTypApplyFun here) t1
            , span [ class "paren" ] [ text " (" ]
            , view_typ opts (FTypApplyArg here) t2
            , span [ class "paren" ] [ text ") "]
            ]
    TArrow t1 t2 ->
        span [ class "typ arrow" ]
            [ view_typ opts (FTypArrowArg here) t1
            , keyword "→"
            , view_typ opts (FTypArrowResult here) t2
            ]


view_qname : Options -> Focus -> QualifiedName -> Html Msg
view_qname opts here (qs, name) = case qs of
  Nothing ->
    span [ class "unqualifiedname" ] [ text (snake opts name) ]
  Just qualifier ->
    span [ class "qualifiedname" ]
      [ span [ class "qualifier" ] [ text (qualifier ++ ".") ]
      , span [ class "unqualifiedname" ] [ text (snake opts name) ]
      ]

snake : Options -> String -> String
snake opts str =
    if not opts.snake
    then str
    else str
        |> String.toList
        |> List.indexedMap (\i c ->
            if i /= 0 && Char.isUpper c then ['-', Char.toLower c] else [c] )
        |> List.concat
        |> String.fromList


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
        span [ class "exp var" ] [ view_qname opts (FVar here) qname ]
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
    in case ( opts.infix, f ) of
        ( True, Apply (Var (qs, fn)) y ) ->
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



to_css : List (String, String) -> String
to_css style =
    style
    |> List.map (\(k, v) -> k ++ ": " ++ v ++ ";\n")
    |> String.concat


css : Model -> String
css model = """

.module {
  --width: 50%;

}

span {
  display: inline-block;
  padding: 1px;
  font-family: Sans;
  font-size: 1.0em;
  font-style: normal;
  """ ++ to_css (bg base00) ++ """
}

.module > div > div > div {
  """ ++ to_css (bg base00) ++ """
  margin-top: 5px;
  border: 0;
}

.exp.var {
  """ ++ to_css (fg base07) ++ """
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
  """ ++ to_css (c base0A) ++ """
  border-style: dashed;
  border-collapse: collapse;
}

.exp.if > tr > td {
  """ ++ to_css (c base0A) ++ """
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
  """ ++ to_css (bc base03) ++ """
}

.exp.apply.op {
  """ ++ to_css (bc base04) ++ """
}

.typ, .typ.name {
  """ ++ to_css (c base0B) ++ """
}

.lit {
  """ ++ to_css (c base06) ++ """
  """ ++ to_css (bg base02) ++ """
  border-radius: 4px;
}

.name, .binding, .exp.lam, .typ.var{
  """ ++ to_css (c base0E) ++ """
}

.exp.lam > .name, .typ.var {
  font-family: Serif;
  font-style: italic;
  font-size: 1.2em;
}

.keyword {
  """ ++ to_css (c base0A) ++ """
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
  """ ++ to_css (c base07) ++ """
}

.exp.var > .unqualifiedname, .typ.var {
  """ ++ to_css (c base0D) ++ """
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
  """ ++ to_css (bg base00) ++ """
}

span:hover, td:hover {
  background-color: #354 !important;
}

span.name:hover, span.exp.lit:hover, span.exp.var:hover, span.qualifiedname:hover, span.unqualifiedname:hover {
  background-color: #687 !important;
}
"""
