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
import Basics.Extra exposing (..)


view_source : String -> Html Msg
view_source src =
    Html.textarea
        [ cols 80
        , rows 12
        , style ( bg "black" ++ c "white" )
        , onInput ChangeSrc
        ]
        [ text src ]


view : Model -> Html Msg
view model = div []
  [ Html.node "style" [] [ text css_ ]
  --, div [] [ text (toString model.ast) ]
  , div [ class "source" ] (if model.opts.source then [ view_source model.src ] else [])
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
        [ checkbox "source" Source opts.source
        , checkbox "borders" Borders opts.borders
        , checkbox "parens" Parens opts.parens
        , checkbox "full names" Qualifiers opts.qualifiers
        , checkbox "infix operators" Infix opts.infix
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
                , span [ style (c base02) ] [ text "with " ]
                , span [ style (c base03) ] [ text "Basics, " ]
                , span [ style (c base03) ] [ text "List, " ]
                , span [ style (c base03) ] [ text "Html" ]
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
            , lparen opts
            , view_typ opts (FTypApplyArg here) t2
            , rparen opts
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
      [ if opts.qualifiers && qualifier /= "Basics"
        then span [ class "qualifier" ] [ text (qualifier ++ ".") ]
        else span [] []
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


with_border1 : Options -> List ( String, String )
with_border1 opts =
    if opts.borders
    then [ "border-width" => "1px" ]
    else []



ligature : ( Maybe String, String ) -> ( Maybe String, String )
ligature ( qs, name ) =
    if name == "|>"
    then ( qs, "▷" )
    else ( qs, name )


view_exp : Options -> Focus -> Exp -> Html Msg
view_exp opts here exp =
    let
        it = case exp of
            Let bindings exp ->
                span [ class "exp let", style (with_border1 opts) ]
                    [ keyword "let"
                    , span [] (bindings |> indexedMap (\n b ->
                        view_binding opts (FLetBinding n here) b))
                    , keyword "in"
                    , view_exp opts (FLetExp here) exp
                    ]
            Lam name exp ->
                span [ class "exp lam", style (with_border1 opts) ]
                    [ keyword "λ"
                    , span [ class "name" ] [ text name ]
                    , span [ class "keyword" ] [text " → " ]
                    --, Html.br [] []
                    , view_exp opts (FLamExp here) exp
                    ]
            Apply f x ->
                view_apply opts here f x
            Var qname ->
                span [ class "exp var", style (with_border1 opts) ] [ view_qname opts (FVar here) (ligature qname) ]
            Case exp cases ->
                view_patmat opts here exp cases
            Lit lit ->
                span [ class "exp lit", style (with_border1 opts) ] [ view_literal (FLit here) lit ]
            Record _ ->
                text "record"
    in it


maybe_box : Exp -> List (Html.Attribute Msg)
maybe_box exp =
    case exp of
        Var _ -> []
        Lit _ -> []
        _ -> [ style ["border" => ("1px solid " ++ base03), "border-radius" => "4px" ] ]


paren : String -> Options -> Html Msg
paren str opts =
    span [ style (c base03) ] [ text (if opts.parens then str else "") ]

lparen : Options -> Html Msg
lparen = paren "("

rparen : Options -> Html Msg
rparen = paren ")"


is_infix : String -> Bool
is_infix fn =
    (String.uncons fn |> Maybe.map (Tuple.first >> (λc -> isUpper c || isLower c))) ? True

view_apply : Options -> Focus -> Exp -> Exp -> Html Msg
view_apply opts here f x =
    let render f x =
            span [ class "exp apply" , style (with_border1 opts) ] <|
                [ lparen opts
                , view_exp opts (FApplyFun here) f
                , span [ style (c base03)] [ text "‹" ]
                , view_exp opts (FApplyArg here) x
                , rparen opts
                ]
    in case ( opts.infix, f ) of
        ( True, Apply (Var (qs, fn)) y ) ->
            if is_infix fn
            then render f x
            else span [ class "exp apply op", style (with_border1 opts) ]
                [ lparen opts
                , span (if opts.borders || opts.parens then [] else maybe_box y)
                    [ view_exp opts (FApplyFun (FApplyFun here)) y ]
                --, text " "
                , span [ style (c base03) ] [ text "›" ]
                , span [ style (bc base03 ++ ["border-width" => "1px"])]
                    [ view_exp opts (FApplyFun (FApplyArg here)) (Var ( qs, fn )) ]
                , span [ style (c base03) ] [ text "‹" ]
                --, text " "
                , span (if opts.borders || opts.parens then [] else maybe_box x)
                    [ view_exp opts (FApplyArg here) x ]
                , rparen opts
                ]
        _ -> render f x





view_patmat : Options -> Focus -> Exp -> List ( Pattern, Exp ) -> Html Msg
view_patmat opts here exp cases =
    let view_case n (pat, exp) =
        tr []
            [ td [ style (with_border1 opts ++ [ "position" => "relative" ]) ]
                [ view_pattern (FCasePattern n here) pat
                , span [ class "arrow", style [ "display" => "inline-block", "position" => "absolute", "right" => "-7px" ] ] [ text " → " ]
                ]
            , td [ style (with_border1 opts ++ [ "padding-left" => "6px"]) ] [ view_exp opts (FCaseResult n here) exp ]
            ]
    in table [ class "exp case if" ] <|
        [ tr []
            [ td [ style (with_border1 opts ++ [ "vertical-align" => "middle" ]) ] [ keyword "case" ]
            , td [ style (with_border1 opts) ] [ view_exp opts (FCaseExp here) exp ]
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
    span [ class "pattern", style (fg base07 ++ bg base02 ++ [ "margin-right" => "8px"]) ] [ text <|
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



css_ : String
css_ = """
html {
    background-color: #111;
    color: """ ++ base07 ++ """;
    font-family: sans;
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
  border-style: solid;
  padding: 2px;
  padding-top: 1px;
  padding-bottom: 1px;
  margin: 2px;
  border-radius: 4px;
}

.binding {
  padding: 0;
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
  border-width: 0px;
  border-collapse: collapse;
  --margin: 2px;
  padding: 1px;
}

.exp.if > tr > td:first-child {
  border-right-width: 1px;
}

.exp.if > tr:first-child > td {
  border-bottom-style: solid;
  border-bottom-width: 1px;
}
.exp.if > tr:first-child > td:first-child {
  border-right-style: none;
}


.exp.if > tr > td {
  border-bottom-width: 1px;
}
.exp.if > tr:last-child > td {
  border-bottom-width: 0px;
}

.exp.if > tr:nth_child(2) > td {
  border-top-style: solid;
}


.exp.if > tr:hover > td:first-child, .exp.if > tr:hover > td:first-child > span {
  background-color: transparent;
}

span.arrow {
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


.qualifier {
  color: grey;
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
