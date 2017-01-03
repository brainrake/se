module Translate exposing (..)

import List.Extra exposing (last, init)
import Ast
import Ast.Expression exposing (..)
import Ast.Statement exposing (..)
import Combine exposing (..)
import Lang exposing (..)
import Maybe.Extra exposing ((?))
import Char exposing (isUpper, isLower)

translate: Result (Combine.ParseErr ()) (Combine.ParseOk () (List Ast.Statement.Statement)) -> Module
translate res = case res of
  Ok (state, stream, statements) -> translate_module statements
  Err (state, stream, errs) -> [ ("Error", Nothing, to_str (String.join "\n" errs)) ]


translate_module : List Ast.Statement.Statement -> Module
translate_module ss = case ss of
  (FunctionTypeDeclaration name_ ast_typ) :: (FunctionDeclaration name names ast_exp) :: ss ->
    let lam = λns -> case ns of
          n :: ns -> Lam n (lam ns)
          [] -> translate_exp ast_exp
    in (name, Just (translate_typ ast_typ), lam names) :: translate_module ss
  (FunctionDeclaration name names exp) :: ss ->
    (name, Nothing, to_str exp) :: translate_module ss
  (Comment str) :: ss -> []
  _ :: ss -> translate_module ss
  [] -> []

translate_typ : Ast.Statement.Type -> Typ
translate_typ ast_typ = case ast_typ of
  TypeConstructor q_ast_typ [] -> TName (init q_ast_typ ? [], last q_ast_typ ? "")
  TypeConstructor q_ast_typ (x :: xs) -> TName (init q_ast_typ ? [], last q_ast_typ ? "")
  TypeVariable name -> TVar name
  TypeRecordConstructor typ pairs -> TVar (toString (typ, pairs))
  TypeRecord pairs -> TVar (toString pairs)
  TypeTuple typs -> TVar (toString typs)
  TypeApplication typ1 typ2 -> TArrow (translate_typ typ1) (translate_typ typ2)

translate_exp : Ast.Expression.Expression -> Exp
translate_exp ast_exp = case ast_exp of
  Character char -> Lit (Char char)
  Ast.Expression.String str -> Lit (Lang.String str)
  Integer int -> Lit (Lang.Int int)
  Ast.Expression.Float float -> Lit (Lang.Float float)
  Variable names -> Var (init names ? [], last names ? "")
  List items -> (items |> List.map translate_exp |> List.Extra.foldl1 (\a b -> (Apply (Apply (Var (["Basics"], "::")) a) b))) ? (Var (["Basics"], "[]"))
  Access exp names -> translate_access exp names
  Record pairs -> to_str ("Record", pairs)
  RecordUpdate name pairs -> to_str (toString ("RecordUpdate", name, pairs))
  Ast.Expression.If cond then_ else_ ->
    Lang.If (translate_exp cond) (translate_exp then_) (translate_exp else_)
  Ast.Expression.Let bindings exp  -> to_str ("Let", bindings, exp)
  Ast.Expression.Case exp cases -> to_str ("Case", exp, cases)
  Lambda names exp -> Lam (List.head names ? "") (translate_exp exp)
  Application exp1 exp2 -> Apply (translate_exp exp1) (translate_exp exp2)
  BinOp (Variable (op::[])) exp1 exp2 -> Apply (Apply (Var (["Basics"], op)) (translate_exp exp1)) (translate_exp exp2)
  BinOp op exp1 exp2 -> Apply (Apply (translate_exp op) (translate_exp exp1)) (translate_exp exp2)

translate_access : Ast.Expression.Expression -> List String -> Exp
translate_access exp names = case (exp, names) of
  (Variable (qname :: []), (name :: [])) ->
    if (String.uncons qname |> Maybe.map (Tuple.first >> (λc -> isUpper c))) ? True
    then Var ([ qname ], name)
    else to_str ("Access", exp, names)
  _ -> to_str ("Access", exp, names)

to_str : a -> Exp
to_str s = Lit (Lang.String (toString s))

