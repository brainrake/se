module Lang exposing (..)

type alias Module = List Binding

type alias Binding = (String, Maybe Typ, Exp)

type Typ =
      TName QualifiedName
    | TVar String
    | TApply Typ Typ
    | TArrow Typ Typ

type alias QualifiedName = (List String, String)

type Exp =
      Let (List Binding) (Exp)
    | Lam String Exp
    | Apply Exp Exp
    | Var QualifiedName
    | Lit Literal
    | If Exp Exp Exp
    | Case (List (Pattern, Exp))
    | Dict (List (String, Exp))

type Literal =
      String String
    | Char Char
    | Int Int
    | Float Float


type Pattern =
      PApply (List Pattern)
    | PName String
