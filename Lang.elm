module Lang exposing (..)

import Dict exposing (Dict)


type alias Module =
    { name : String
    , imports : List String
    , bindings : List Binding
    }


type alias Binding =
    ( String, Maybe Typ, Exp )


type Typ
    = TName QualifiedName
    | TVar String
    | TApply Typ Typ
    | TArrow Typ Typ


type alias QualifiedName =
    ( Maybe String, String )


type Exp
    = Let (List Binding) Exp
    | Lam String Exp
    | Apply Exp Exp
    | Var QualifiedName
    | Lit Literal
    | Case Exp (List ( Pattern, Exp ))
    | Record (Dict String Exp)


type Pattern
    = PApply (List Pattern)
    | PCon QualifiedName
    | PVar String
    | PLit Literal


type Literal
    = String String
    | Char Char
    | Int Int
    | Float Float


type Focus
    = FTypApplyFun Focus
    | FTypApplyArg Focus
    | FTypArrowArg Focus
    | FTypArrowResult Focus
    | FTypName Focus
    | FLetBinding Int Focus
    | FLetExp Focus
    | FBindingName Focus
    | FBindingTyp Focus
    | FBindingValue Focus
    | FLamArg Focus
    | FLamExp Focus
    | FApplyFun Focus
    | FApplyArg Focus
    | FVar Focus
    | FLit Focus
    | FCaseExp Focus
    | FCasePattern Int Focus
    | FCaseResult Int Focus
    | FRecordKey String Focus
    | FRecordValue String Focus
    | FPoint
