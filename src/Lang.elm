module Lang exposing (..)

import Json.Encode exposing (..)
import DictList exposing (DictList)


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


encode_exp : Exp -> Json.Encode.Value
encode_exp exp =
    case exp of
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
