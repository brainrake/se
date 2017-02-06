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
    div [ style (bg base00 ++ [ "margin-top" => "5px"]) ]
        (bindings |> map (view_binding opts FPoint))


view_binding : Options -> Focus -> Binding -> Html Msg
view_binding opts here (name, mtyp, exp) =
    let view_binding_with_typ typ =
            [ div [ class "binding_typ" ]
                    [ span [ class "name", style (c base0E) ] [ text (snake opts name) ]
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
        span [ class "typ name", style (c base0B) ] [ view_qname opts (FTypName here) qname ]
    TVar var ->
        span [ class "typ var", style (var_style ++ c base0D) ] [ text (snake opts var) ]
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
view_qname opts here (qs, name) =
    let color = case here of
        FTypName _ -> base0B
        _ -> base07
    in case qs of
        Nothing ->
            span [ class "unqualifiedname", style (var_style ++ c base0D) ] [ text (snake opts name) ]
        Just qualifier ->
            span [ class "qualifiedname", style (c base03) ]
                [   if opts.qualifiers && qualifier /= "Basics"
                    then span [ class "qualifier" ] [ text (qualifier ++ ".") ]
                    else span [] []
                , span [ class "unqualifiedname", style (c color) ] [ text (snake opts name) ]
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
    [ "border-width" => (if opts.borders then "1px" else "0") ]


ligature : ( Maybe String, String ) -> ( Maybe String, String )
ligature ( qs, name ) =
    if name == "|>"
    then ( qs, "▷" )
    else ( qs, name )


var_style : List ( String, String )
var_style =
    [ "font-family" => "Serif", "font-style" => "italic", "font-size" => "1.2em"]


lit_style : List ( String, String )
lit_style =
    (c base06) ++ (bg base02) ++ [ "border-radius" => "4px"]


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
                span [ class "exp lam", style (border_style ++ c base0E ++ with_border1 opts) ]
                    [ keyword "λ"
                    , span [ class "name", style var_style ] [ text name ]
                    , keyword "→"
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
                span [ class "exp lit", style (lit_style ++ with_border1 opts) ] [ view_literal (FLit here) lit ]
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
                , span (if opts.parens then [] else maybe_box x)
                    [ view_exp opts (FApplyArg here) x ]
                , rparen opts
                ]
    in case ( opts.infix, f ) of
        ( True, Apply (Var (qs, fn)) y ) ->
            if is_infix fn
            then render f x
            else span [ class "exp apply op", style (with_border1 opts) ]
                [ lparen opts
                , span (if opts.parens then [] else maybe_box y)
                    [ view_exp opts (FApplyFun (FApplyFun here)) y ]
                --, text " "
                , span [ style (c base03) ] [ text "›" ]
                , span [ style (bc base03 ++ ["border-width" => "1px"])]
                    [ view_exp opts (FApplyFun (FApplyArg here)) (Var ( qs, fn )) ]
                , span [ style (c base03) ] [ text "‹" ]
                --, text " "
                , span (if opts.parens then [] else maybe_box x)
                    [ view_exp opts (FApplyArg here) x ]
                , rparen opts
                ]
        _ -> render f x


border_style : List ( String, String )
border_style =
    [ "border-style" => "solid"
    , "--padding" => "2px"
    , "--padding-top" => "1px"
    , "--padding-bottom" => "1px"
    , "--margin" => "2px"
    , "border-radius" => "4px"
    ]

pat_arrow_style : List ( String, String )
pat_arrow_style =
    [ "display" => "inline-block"
    , "position" => "absolute"
    , "right" => "-7px"
    , "background-color" => "transparent"
    ]

td_style : List ( String, String )
td_style =
    bg base00 ++ c base0A ++
    [ "vertical-align" => "top"
    , "padding" => "1px"
    , "font-family" => "Sans"
    , "font-size" => "1.0em"
    , "font-style" => "normal"
    , "border-style" => "dashed"
    , "border-width" => "0px"
    , "border-collapse" => "collapse"
    , "padding" => "1px"
    ]

view_patmat : Options -> Focus -> Exp -> List ( Pattern, Exp ) -> Html Msg
view_patmat opts here exp cases =
    let view_case n (pat, exp) =
        tr []
            [ td [ style (with_border1 opts ++ td_style ++ [ "position" => "relative", "border-right-width" => "1px" ]) ]
                [ view_pattern (FCasePattern n here) pat
                , span [ class "arrow", style pat_arrow_style ] [ text " → " ]
                ]
            , td [ style (with_border1 opts ++ td_style ++ [ "padding-left" => "6px", "border-top-width" => "1px" ]) ]
                [ view_exp opts (FCaseResult n here) exp ]
            ]
    in table [ class "exp case if" ] <|
        [ tr []
            [ td [ style (with_border1 opts ++ td_style ++ [ "vertical-align" => "middle", "border-bottom-style" => "solid",
  "border-bottom-width" => "1px" ]) ] [ keyword "case" ]
            , td [ style (with_border1 opts ++ td_style) ] [ view_exp opts (FCaseExp here) exp ]
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
    span [ class "keyword", style (c base0A) ] [ text (" " ++ str ++ " ") ]



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
}
"""
