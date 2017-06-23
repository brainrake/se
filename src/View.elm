module View exposing (view)

import Html exposing (Html, div, span, text, pre, button, table, thead, tbody, th, tr, td, input, label, pre)
import Html.Attributes exposing (class, style, rows, cols, type_, checked)
import Html.Events exposing (onInput)
import Html.Events exposing (onClick)
import String exposing (join)
import List exposing (map, isEmpty, indexedMap)
import Char exposing (isUpper, isLower)
import Maybe.Extra exposing ((?))
import Lang exposing (..)
import Model exposing (..)
import Basics.Extra exposing ((=>))
import DictList exposing (DictList)
import SeColor exposing (..)
import SeRender exposing (..)
import Json.Decode


--import Json.Encode exposing (encode)


type alias Context =
    { opts : Options
    , here : List Focus
    , cursor : List Focus
    , pointer : List Focus
    , is_pattern : Bool
    }


init_ctx : Context
init_ctx =
    { opts = init_opts
    , here = []
    , cursor = []
    , pointer = []
    , is_pattern = False
    }


zoom : Focus -> Context -> Context
zoom focus ctx =
    { ctx
        | here = focus :: ctx.here
        , pointer =
            case ctx.pointer of
                [] ->
                    []

                x :: xs ->
                    if x == focus then
                        xs
                    else
                        []
        , cursor =
            case ctx.cursor of
                [] ->
                    []

                x :: xs ->
                    if x == focus then
                        xs
                    else
                        []
    }


pointer_msgs : Context -> List (Html.Attribute Msg)
pointer_msgs ctx =
    [ Html.Events.onMouseEnter (ChangePointer (List.reverse (FPoint :: ctx.here)))
    , Html.Events.onMouseLeave (ChangePointer (List.reverse (FPoint :: (List.tail ctx.here ? []))))
    , Html.Events.onWithOptions "click" (Html.Events.Options True True) (Json.Decode.succeed (ChangeCursor (List.reverse (FPoint :: ctx.here))))
    ]


pointer_style : Context -> List ( String, String )
pointer_style ctx =
    let
        style1 =
            case ctx.pointer of
                FPoint :: [] ->
                    [ "opacity" => "1", "background-color" => "#696", "border-color" => "#afa" ]

                --_ :: _ ->
                --    [ "opacity" => "1", "border-color" => "#696", "border-width" => "1px" ]
                _ ->
                    []

        style2 =
            case ctx.cursor of
                FPoint :: [] ->
                    [ "opacity" => "1", "background-color" => "#66a", "border-color" => "#aaf" ]

                --_ :: _ ->
                --    [ "opacity" => "1", "border-color" => "#669", "border-width" => "1px" ]
                _ ->
                    []
    in
        style1 ++ style2



--["opacity" => "0.7"]


view_source : String -> Html Msg
view_source src =
    Html.textarea
        [ cols 80
        , rows 12
        , style (bg "black" ++ c "white")
        , onInput ChangeSrc
        ]
        [ text src ]


view : Model -> Html Msg
view model =
    div []
        [ Html.node "style" [] [ text css_ ]
        , Html.node "script" [] [ text """window.addEventListener("keydown", (e) => { if (e.key.indexOf("Arrow") != -1 ) { e.preventDefault() }})""" ]
          --, div [] [ text (toString model.ast) ]
        , div []
            (if model.opts.source then
                [ view_source model.src ]
             else
                []
            )
        , div [] [ view_config model.opts |> Html.map OptionsMsg ]
        , div [ Html.Events.onMouseLeave (ChangePointer []) ]
            [ view_module
                { init_ctx
                    | opts = model.opts
                    , pointer = model.pointer
                    , cursor = model.cursor
                }
                model.ast
            ]
        , div [] [ text (toString model.pointer) ]
        , div [] [ text (toString model.cursor) ]
          --, div [] [ text (toString model.ast) ]
          --, div [] [ text (encode 4 (encode_bindings model.ast.bindings)) ]
        , pre [] [ text (str_bindings 0 model.ast.bindings) ]
          --, div [] [ text (toString model.keys_pressed) ]
          --, div [ class "module" ] [ view_module model.module_ ]
        ]


checkbox : String -> (Bool -> msg) -> Bool -> Html msg
checkbox name msg val =
    label
        [ style [ ( "padding", "20px" ) ] ]
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
        , checkbox "direction" Direction opts.direction
        ]


view_module : Context -> Module -> Html Msg
view_module ctx { name, imports, bindings } =
    div [ style ([ "margin-top" => "5px" ]) ]
        --bg base00 ++
        [ (bindings |> view_bindings ctx) ]


view_bindings : Context -> Bindings -> Html Msg
view_bindings ctx bindings =
    let
        view_binding_with_typ typ n name exp =
            [ div []
                [ span [ style (c base0E ++ pointer_style (zoom (FBindingName n) ctx)) ] [ text (snake ctx.opts name) ]
                , keyword ":"
                , view_typ (zoom (FBindingTyp n) ctx) typ
                  --, keyword ""
                  --, span [ style (c base02) ] [ text "with " ]
                  --, span [ style (c base03) ] [ text "Basics, " ]
                  --, span [ style (c base03) ] [ text "List, " ]
                  --, span [ style (c base03) ] [ text "Html " ]
                , keyword "="
                ]
            , div [] [ view_exp (zoom (FBindingValue n) ctx) exp ]
            ]

        view_binding_no_typ n name exp =
            [ span [] [ text (snake ctx.opts name) ]
            , keyword "="
            , view_exp (zoom (FBindingValue n) ctx) exp
            ]
    in
        div [ style (pointer_style ctx) ] <|
            (bindings
                |> DictList.indexedMap
                    (\n name ( mtyp, exp ) ->
                        div [ style [ "border" => "6px solid black" ] ] <|
                            case mtyp of
                                Just typ ->
                                    view_binding_with_typ typ n name exp

                                Nothing ->
                                    view_binding_no_typ n name exp
                    )
                |> DictList.toList
                |> List.map Tuple.second
            )


view_typ : Context -> Typ -> Html Msg
view_typ ctx typ =
    case typ of
        TCons qname [] ->
            span (pointer_msgs ctx ++ [ style (c base0B ++ pointer_style ctx) ])
                [ view_qname (zoom FTypName ctx) qname ]

        TCons qname args ->
            span (pointer_msgs ctx)
                [ lparen ctx.opts
                , view_qname (zoom FTypName ctx) qname
                , span [] <| (args |> map (view_typ (zoom FTypApplyArg ctx)))
                , rparen ctx.opts
                ]

        TVar var ->
            span (pointer_msgs ctx ++ [ style (pointer_style ctx ++ var_style ++ c base0D) ]) [ text (snake ctx.opts var) ]

        TArrow t1 t2 ->
            span (pointer_msgs ctx ++ [ style (pointer_style ctx) ])
                [ view_typ (zoom FTypArrowArg ctx) t1
                , keyword "→"
                , view_typ (zoom FTypArrowResult ctx) t2
                ]

        TRecord record ->
            span (pointer_msgs ctx ++ [ style (pointer_style ctx) ]) [ text (toString record) ]


view_qname : Context -> QualifiedName -> Html Msg
view_qname ctx ( qs, name ) =
    let
        color =
            case ctx.here of
                FTypName :: _ ->
                    base0B

                _ ->
                    base07
    in
        case qs of
            Nothing ->
                span (pointer_msgs ctx ++ [ style (pointer_style ctx ++ var_style ++ c base0D) ])
                    [ text (snake ctx.opts name) ]

            Just qualifier ->
                span (pointer_msgs ctx ++ [ style (pointer_style ctx ++ c base03) ])
                    [ if ctx.opts.qualifiers && qualifier /= "Basics" then
                        span [] [ text (qualifier ++ ".") ]
                      else
                        span [] []
                    , span [ style (c color) ] [ text (snake ctx.opts name) ]
                    ]


snake : Options -> String -> String
snake opts str =
    if not opts.snake then
        str
    else
        str
            |> String.toList
            |> List.indexedMap
                (\index char ->
                    if Char.isUpper char && index > 0 then
                        [ '‑', Char.toLower char ]
                    else
                        [ char ]
                )
            |> List.concat
            |> String.fromList


with_border1 : Options -> List ( String, String )
with_border1 opts =
    [ "border-width"
        => (if opts.borders then
                "1px"
            else
                "0"
           )
    ]


ligature : ( Maybe String, String ) -> ( Maybe String, String )
ligature ( qs, name ) =
    if name == "|>" then
        ( qs, "▷" )
    else if name == "<|" then
        ( qs, "◁" )
    else
        ( qs, name )


var_style : List ( String, String )
var_style =
    [ "font-family" => "Serif", "font-style" => "italic", "font-size" => "1.2em" ]


lit_style : List ( String, String )
lit_style =
    (c base06) ++ (bg base02) ++ [ "border-radius" => "4px" ]


view_exp : Context -> Exp -> Html Msg
view_exp ctx exp =
    let
        pattern_error =
            span [] [ keyword "PATTERN ERROR" ]

        it =
            case exp of
                Let bindings exp ->
                    if ctx.is_pattern then
                        pattern_error
                    else
                        span (pointer_msgs ctx ++ [ style (pointer_style ctx ++ with_border1 ctx.opts) ])
                            [ keyword "let"
                            , view_bindings (zoom FLetBindings ctx) bindings
                            , keyword "in"
                            , view_exp (zoom FLetExp ctx) exp
                            ]

                Lam name exp ->
                    if ctx.is_pattern then
                        pattern_error
                    else
                        span [ style (border_style ++ c base0E ++ with_border1 ctx.opts) ]
                            [ lparen ctx.opts
                            , if True then
                                -- ctx.opts.parens then
                                keyword "λ"
                              else
                                text ""
                            , span (pointer_msgs (zoom FLamArg ctx) ++ [ class "name", style (var_style ++ pointer_style (zoom FLamArg ctx)) ]) [ text name ]
                            , if ctx.opts.parens then
                                keyword ""
                              else
                                keyword "→"
                              --, Html.br [] []
                            , view_exp (zoom FLamExp ctx) exp
                            , rparen ctx.opts
                            ]

                Apply f x ->
                    view_apply ctx f x

                Var qname ->
                    span (pointer_msgs ctx ++ [ style (pointer_style ctx ++ with_border1 ctx.opts) ])
                        [ view_qname ctx (ligature qname) ]

                Case exp cases ->
                    if ctx.is_pattern then
                        pattern_error
                    else
                        view_patmat ctx exp cases

                Lit lit ->
                    span (pointer_msgs ctx ++ [ style (pointer_style ctx ++ lit_style ++ with_border1 ctx.opts) ])
                        [ view_literal ctx lit ]

                Tup t ->
                    span []
                        ([ keyword "(" ]
                            ++ (t
                                    |> List.indexedMap
                                        (\n exp ->
                                            view_exp (zoom (FTup n) ctx) exp
                                        )
                                    |> List.intersperse
                                        (keyword ",")
                               )
                            ++ [ keyword ")" ]
                        )

                Record r ->
                    text (toString r)
    in
        it


maybe_box : Exp -> List (Html.Attribute Msg)
maybe_box exp =
    case exp of
        Var _ ->
            []

        Lit _ ->
            []

        _ ->
            [ style [ "border" => ("1px solid " ++ base03), "border-radius" => "4px" ] ]


paren : String -> Options -> Html Msg
paren str opts =
    span [ style (c base03) ]
        [ text
            (if opts.parens then
                str
             else
                ""
            )
        ]


lparen : Options -> Html Msg
lparen =
    paren "("


rparen : Options -> Html Msg
rparen =
    paren ")"


view_apply : Context -> Exp -> Exp -> Html Msg
view_apply ctx f x =
    let
        render f x =
            span [ class "exp apply", style (with_border1 ctx.opts ++ pointer_style ctx) ] <|
                [ lparen ctx.opts
                , view_exp (zoom FApplyFun ctx) f
                , span [ style (c base03) ]
                    [ text
                        (if ctx.opts.direction then
                            "‹"
                         else
                            ""
                        )
                    ]
                , span
                    (if ctx.opts.parens then
                        []
                     else
                        maybe_box x
                    )
                    [ view_exp (zoom FApplyArg ctx) x ]
                , rparen ctx.opts
                ]
    in
        case ( ctx.opts.infix, f ) of
            ( True, Apply (Var ( qs, fn )) y ) ->
                if not (is_infix fn) then
                    render f x
                else
                    span [ style (with_border1 ctx.opts) ]
                        [ lparen ctx.opts
                        , span
                            (if ctx.opts.parens then
                                []
                             else
                                maybe_box y
                            )
                            [ view_exp (zoom FApplyFun (zoom FApplyFun ctx)) y ]
                          --, text " "
                        , span [ style (c base03) ]
                            [ text
                                (if ctx.opts.direction then
                                    "›"
                                 else
                                    ""
                                )
                            ]
                        , span [ style (bc base03 ++ [ "border-width" => "1px" ]) ]
                            [ view_exp (zoom FApplyFun (zoom FApplyArg ctx)) (Var ( qs, fn )) ]
                        , span [ style (c base03) ]
                            [ text
                                (if ctx.opts.direction then
                                    "‹"
                                 else
                                    ""
                                )
                            ]
                          --, text " "
                        , span
                            (if ctx.opts.parens then
                                []
                             else
                                maybe_box x
                            )
                            [ view_exp (zoom FApplyArg ctx) x ]
                        , rparen ctx.opts
                        ]

            _ ->
                render f x


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
    c base0A
        ++ [ "vertical-align" => "top"
           , "padding" => "1px"
           , "font-family" => "Sans"
           , "font-size" => "1.0em"
           , "font-style" => "normal"
           , "border-style" => "dashed"
           , "border-width" => "0px"
           , "border-collapse" => "collapse"
           , "padding" => "1px"
           ]


view_patmat : Context -> Exp -> List ( Exp, Exp ) -> Html Msg
view_patmat ctx exp cases =
    let
        view_case n ( pat, exp ) =
            tr []
                [ td [ style ((pointer_style (zoom (FCasePattern n) ctx)) ++ with_border1 ctx.opts ++ td_style ++ [ "position" => "relative", "border-right-width" => "1px" ]) ]
                    [ view_exp (zoom (FCasePattern n) { ctx | is_pattern = True }) pat
                    , span [ style pat_arrow_style ] [ text " → " ]
                    ]
                , td [ style (with_border1 ctx.opts ++ td_style ++ [ "padding-left" => "6px", "border-top-width" => "1px" ]) ]
                    [ view_exp (zoom (FCaseResult n) ctx) exp ]
                ]
    in
        table (pointer_msgs ctx ++ []) <|
            [ tr []
                [ td
                    [ style
                        (with_border1 ctx.opts
                            ++ td_style
                            ++ [ "vertical-align" => "middle"
                               , "border-bottom-style" => "solid"
                               , "border-bottom-width" => "1px"
                               ]
                        )
                    ]
                    [ keyword "case" ]
                , td [ style (with_border1 ctx.opts ++ td_style) ]
                    [ view_exp (zoom FCaseExp ctx) exp ]
                ]
            ]
                ++ (cases |> indexedMap view_case)


view_literal : Context -> Literal -> Html Msg
view_literal ctx lit =
    span (pointer_msgs ctx ++ [ style (pointer_style ctx) ]) <|
        case lit of
            String lit ->
                [ text ("\"" ++ lit ++ "\"") ]

            Char lit ->
                [ text ("'" ++ toString lit ++ "'") ]

            Int lit ->
                [ text (toString lit) ]

            Float lit ->
                [ text (toString lit) ]


keyword : String -> Html a
keyword str =
    span [ style (c base0A) ] [ text (" " ++ str ++ " ") ]


to_css : List ( String, String ) -> String
to_css style =
    style
        |> List.map (\( k, v ) -> k ++ ": " ++ v ++ ";\n")
        |> String.concat
