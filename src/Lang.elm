module Lang exposing (..)

import Json.Encode exposing (..)
import DictList exposing (DictList)
import Char exposing (isUpper, isLower)
import Maybe.Extra exposing ((?))


type alias TypName =
    String


type alias ModuleName =
    String


type alias FieldName =
    String


type alias ConsName =
    String


type alias Module =
    { name : ModuleName
    , imports : List ModuleName
    , types : DictList TypName TypDef
    , aliases : DictList TypName Typ
    , bindings : Bindings
    }


init_module : Module
init_module =
    { name = "Unnamed"
    , imports = []
    , types = DictList.empty
    , aliases = DictList.empty
    , bindings = DictList.empty
    }


type alias TypDef =
    List ( ConsName, List Typ )


type Typ
    = TCons QualifiedName (List Typ)
    | TArrow Typ Typ
    | TRecord (DictList String Typ)
    | TVar String


type alias QualifiedName =
    ( Maybe String, String )


type alias Bindings =
    DictList FieldName ( Maybe Typ, Exp )


type Exp
    = Apply Exp Exp
    | Let Bindings Exp
    | Lam FieldName Exp
    | Var QualifiedName
    | Lit Literal
    | Tup (List Exp)
    | Case Exp (List ( Exp, Exp ))
    | Record Bindings



--| Field FieldName


type Literal
    = String String
    | Char Char
    | Int Int
    | Float Float


type Focus
    = FTypApplyFun
    | FTypApplyArg
    | FTypArrowArg
    | FTypArrowResult
    | FTypName
    | FLetBindings
    | FLetExp
    | FBindingName Int
    | FBindingTyp Int
    | FBindingValue Int
    | FLamArg
    | FLamExp
    | FApplyFun
    | FApplyArg
    | FCaseExp
    | FCasePattern Int
    | FCaseResult Int
    | FRecordKey String
    | FRecordValue String
    | FTup Int
    | FPoint


is_infix : String -> Bool
is_infix fn =
    not ((String.uncons fn |> Maybe.map (Tuple.first >> (\c -> isUpper c || isLower c))) ? True)


encode_exp : Exp -> Json.Encode.Value
encode_exp exp_ =
    case exp_ of
        Apply fun arg ->
            object [ ( "Apply", list [ encode_exp fun, encode_exp arg ] ) ]

        Let bindings exp ->
            object [ ( "Let", list [ encode_bindings bindings, encode_exp exp ] ) ]

        Lam arg exp ->
            object [ ( "Lam", list [ string arg, encode_exp exp ] ) ]

        Var name ->
            object [ ( "Var", string <| toString name ) ]

        Lit lit ->
            object [ ( "Lit", encode_literal lit ) ]

        Tup exps ->
            object [ ( "Tup", list (List.map encode_exp exps) ) ]

        Case exp cases ->
            object [ ( "Case", list [ encode_exp exp, list (cases |> List.map (\( p, v ) -> list [ encode_exp p, encode_exp v ])) ] ) ]

        Record bindings ->
            object [ ( "Record", encode_bindings bindings ) ]


encode_literal : Literal -> Json.Encode.Value
encode_literal lit =
    case lit of
        String x ->
            string x

        Char x ->
            string (toString x)

        Int x ->
            int x

        Float x ->
            float x


encode_bindings : Bindings -> Json.Encode.Value
encode_bindings bs =
    list (DictList.toList bs |> List.map (\( k, ( t, v ) ) -> list [ string k, encode_exp v ]))


indent : Int -> String
indent level =
    List.repeat level "    " |> String.join ""


str_exp : Int -> Exp -> String
str_exp level exp_ =
    case exp_ of
        Apply (Apply (Var ( ns, f )) arg1) arg2 ->
            if is_infix f then
                "(" ++ str_exp level arg1 ++ " " ++ str_exp level (Var ( ns, f )) ++ " " ++ str_exp level arg2 ++ ")"
            else
                "((" ++ str_exp level (Var ( ns, f )) ++ " " ++ str_exp level arg1 ++ ") " ++ str_exp level arg2 ++ ")"

        Apply fun arg ->
            "(" ++ str_exp level fun ++ " " ++ str_exp level arg ++ ")"

        Let bindings exp ->
            indent level ++ "let" ++ (bindings |> str_bindings (level + 1)) ++ "\n" ++ indent level ++ "in\n" ++ str_exp (level + 1) exp

        Lam arg exp ->
            "(\\" ++ arg ++ " ->\n" ++ indent (level + 1) ++ str_exp (level + 1) exp ++ ")"

        Var name ->
            Tuple.second name

        Lit lit ->
            str_literal lit

        Tup exps ->
            "( " ++ (exps |> List.map (str_exp level) |> String.join ", ") ++ " )"

        Case exp cases ->
            "case "
                ++ str_exp level exp
                ++ " of\n"
                ++ (cases
                        |> List.map
                            (\( p, res ) ->
                                indent (level + 1)
                                    ++ str_exp (level + 1) p
                                    ++ " -> \n"
                                    ++ indent (level + 2)
                                    ++ str_exp (level + 2) res
                            )
                        |> String.join "\n"
                   )

        Record bindings ->
            "record"


str_literal : Literal -> String
str_literal lit =
    case lit of
        String x ->
            "\"" ++ x ++ "\""

        Char x ->
            "'" ++ toString x ++ "'"

        Int x ->
            toString x

        Float x ->
            toString x


str_bindings : Int -> Bindings -> String
str_bindings level bs =
    bs
        |> DictList.toList
        |> List.map
            (\( name, ( m_typ, exp ) ) ->
                indent level ++ name ++ " = \n" ++ indent (level + 1) ++ str_exp (level + 1) exp
            )
        |> String.join "\n"
        |> (++) "\n"
