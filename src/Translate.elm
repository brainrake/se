module Translate exposing (..)

import List exposing (head)
import List.Extra exposing (last, init)


--import Ast

import Ast.Expression exposing (..)
import Ast.Statement exposing (..)
import Combine exposing (..)
import Lang exposing (..)
import Maybe.Extra exposing ((?))
import Char exposing (isUpper, isLower)
import DictList exposing (DictList)


translate : Result (Combine.ParseErr ()) (Combine.ParseOk () (List Ast.Statement.Statement)) -> Module
translate res =
    case res of
        Ok ( state, stream, statements ) ->
            translate_module statements

        Err ( state, stream, errs ) ->
            { name = "Error"
            , imports = []
            , types = DictList.empty
            , aliases = DictList.empty
            , bindings = DictList.singleton "Error" ( Nothing, to_str ( state, stream, errs ) )
            }


translate_module : List Ast.Statement.Statement -> Module
translate_module ss =
    let
        translate ss =
            case ss of
                (FunctionTypeDeclaration name_ ast_typ) :: (FunctionDeclaration name exps ast_exp) :: ss ->
                    let
                        lam =
                            \ns ->
                                case ns of
                                    (Ast.Expression.Variable (n :: [])) :: ns ->
                                        Lam n (lam ns)

                                    exp :: ns ->
                                        to_str ( "Lambda", ns, exp )

                                    [] ->
                                        translate_exp ast_exp
                    in
                        ( name, ( Just (translate_typ ast_typ), lam exps ) ) :: translate ss

                (FunctionDeclaration name names exp) :: ss ->
                    ( name, ( Nothing, to_str exp ) ) :: translate ss

                (Comment str) :: ss ->
                    []

                _ :: ss ->
                    translate ss

                [] ->
                    []
    in
        { name = "Main"
        , imports = []
        , types = DictList.empty
        , aliases = DictList.empty
        , bindings = DictList.fromList (translate ss)
        }


translate_typ : Ast.Statement.Type -> Typ
translate_typ ast_typ =
    case ast_typ of
        TypeConstructor q_ast_typ args ->
            TCons ( init q_ast_typ |> Maybe.map String.concat, last q_ast_typ ? "" ) (args |> List.map translate_typ)

        TypeVariable name ->
            TVar name

        TypeRecordConstructor typ pairs ->
            TVar (toString ( typ, pairs ))

        TypeRecord pairs ->
            TVar (toString pairs)

        TypeTuple typs ->
            TVar (toString typs)

        TypeApplication typ1 typ2 ->
            TArrow (translate_typ typ1) (translate_typ typ2)


translate_exp : Ast.Expression.Expression -> Exp
translate_exp ast_exp =
    case ast_exp of
        Character char ->
            Lit (Char char)

        Ast.Expression.String str ->
            Lit (Lang.String str)

        Integer int ->
            Lit (Lang.Int int)

        Ast.Expression.Float float ->
            Lit (Lang.Float float)

        Variable names ->
            Var
                ( if List.length names > 1 then
                    head names
                  else
                    Nothing
                , last names ? ""
                )

        List items ->
            (items
                |> List.map translate_exp
                |> List.Extra.foldl1
                    (\a b ->
                        (Apply (Apply (Var ( Just "Basics", "::" )) a) b)
                    )
            )
                ? (Var ( Just "Basics", "[]" ))

        Access exp names ->
            translate_access exp names

        Ast.Expression.Record pairs ->
            to_str ( "Record", pairs )

        RecordUpdate name pairs ->
            to_str ( "RecordUpdate", name, pairs )

        Ast.Expression.If cond then_ else_ ->
            Lang.Case (translate_exp cond)
                [ ( Var ( Just "Basics", "True" ), translate_exp then_ )
                , ( Var ( Just "Basics", "False" ), translate_exp else_ )
                ]

        Ast.Expression.Let bindings exp ->
            to_str ( "Let", bindings, exp )

        Ast.Expression.Case exp cases ->
            Lang.Case (translate_exp exp)
                (List.map (\( p, exp ) -> ( translate_exp p, translate_exp exp )) cases)

        Lambda exps exp ->
            -- TODO - other args
            case List.head exps of
                Just (Ast.Expression.Variable (name :: [])) ->
                    Lam name (translate_exp exp)

                _ ->
                    to_str ( "Lambda", exps, exp )

        Application exp1 exp2 ->
            Apply (translate_exp exp1) (translate_exp exp2)

        BinOp (Variable (op :: [])) exp1 exp2 ->
            Apply (Apply (Var ( Just "Basics", op )) (translate_exp exp1)) (translate_exp exp2)

        BinOp op exp1 exp2 ->
            Apply (Apply (translate_exp op) (translate_exp exp1)) (translate_exp exp2)

        Tuple t ->
            Tup (t |> List.map (\exp -> translate_exp exp))

        AccessFunction af ->
            to_str ( "AccessFunction", "", af )


translate_access : Ast.Expression.Expression -> List String -> Exp
translate_access exp names =
    case ( exp, names ) of
        ( Variable (qname :: []), name :: [] ) ->
            if (String.uncons qname |> Maybe.map (Tuple.first >> (\c -> isUpper c))) ? True then
                Var ( (Just qname), name )
            else
                to_str ( "Access", exp, names )

        _ ->
            to_str ( "Access", exp, names )


to_str : a -> Exp
to_str s =
    Lit (Lang.String (toString s))
