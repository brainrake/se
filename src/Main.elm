module Main exposing (..)

--import TimeTravel.Html exposing (beginnerProgram)

import Html exposing (programWithFlags)
import Keyboard.Extra exposing (Key(..))
import View exposing (view)
import Model exposing (..)
import Translate exposing (translate)
import Lang exposing (..)
import DictList exposing (..)
import List.Extra exposing ((!!))
import Ast


fbstr : String
fbstr =
    """
main : Html.Html msg
main = Html.ul [] (List.range 1 100 |> List.map (\\n ->
  Html.li [] [ Html.text (Main.fizzBuzz n) ]))


fizzBuzz : Int -> String
fizzBuzz n =
    case (n % 3, n % 5) of
        (0, 0) -> "fizzbuzz"
        (0, _) -> "fizz"
        (_, 0) -> "buzz"
        _ -> Basics.toString n
"""


c : String
c =
    """
    case (n % 3, n % 5) of
        (0, 0) -> "fizzbuzz"
        (0, _) -> "fizz"
        (_, 0) -> "buzz"
        _ ->  Basics.toString n)

"""


b : String
b =
    """
fizzBuzz : Int -> String
fizzBuzz n = (
    if n % 3 == 0 && n % 5 == 0 then "fizzbuzz"
    else if n % 3 == 0 then "fizz"
    else if n % 5 == 0 then "buzz"
    else Basics.toString n)

"""


a : String
a =
    """
fizzbuzz : Int -> String
fizzbuzz n =
    case ( n % 3 == 0, n % 5 == 0) of
      ( True, True ) -> "fizzbuzz"
      ( True, False ) -> "fizz"
      ( False, True ) -> "buzz"
      ( False, False ) -> Basics.toString n
"""


init : Model
init =
    { init_model
        | src = fbstr
        , ast = translate (Ast.parse fbstr)
    }


update_opts : OptionsMsg -> Options -> Options
update_opts msg opts =
    case msg of
        Source x ->
            { opts | source = x }

        Borders x ->
            { opts | borders = x }

        Parens x ->
            { opts | parens = x }

        Qualifiers x ->
            { opts
                | qualifiers = x
            }

        Infix x ->
            { opts | infix = x }

        Snake x ->
            { opts | snake = x }

        Direction x ->
            { opts | direction = x }


get_last : List Focus -> Maybe Focus
get_last at =
    case at of
        focus :: FPoint :: [] ->
            Just focus

        _ :: xs ->
            get_last xs

        [] ->
            Nothing


update_last : Focus -> List Focus -> List Focus -> List Focus
update_last focus at acc =
    case at of
        _ :: FPoint :: _ ->
            case focus of
                FPoint ->
                    acc ++ [ FPoint ]

                _ ->
                    acc ++ [ focus, FPoint ]

        FPoint :: _ ->
            (acc ++ [ FPoint ])

        x :: xs ->
            update_last focus xs (acc ++ [ x ])

        [] ->
            acc ++ [ FPoint ]


remove_last : List Focus -> List Focus -> List Focus
remove_last xs acc =
    case xs of
        _ :: FPoint :: _ ->
            List.reverse (FPoint :: acc)

        x :: ys ->
            remove_last ys (x :: acc)

        [] ->
            List.reverse (FPoint :: acc)


remove_point : List Focus -> List Focus -> List Focus
remove_point xs acc =
    case xs of
        FPoint :: _ ->
            List.reverse acc

        x :: ys ->
            remove_point ys (x :: acc)

        [] ->
            List.reverse acc


walk_bindings : Bindings -> List Focus -> Maybe Exp
walk_bindings bindings at =
    case at of
        (FBindingValue n) :: ats ->
            bindings |> DictList.getAt n |> Maybe.andThen (Tuple.second >> Tuple.second >> \exp -> walk_exp exp ats)

        [ FPoint ] ->
            Just (Record bindings)

        _ ->
            Nothing


walk_exp : Exp -> List Focus -> Maybe Exp
walk_exp exp_ at_ =
    case ( exp_, at_ ) of
        ( Apply fun arg, FApplyFun :: at ) ->
            walk_exp fun at

        ( Apply fun arg, FApplyArg :: at ) ->
            walk_exp arg at

        ( Let bindings exp, FLetBindings :: at ) ->
            walk_bindings bindings at

        ( Let bindings exp, FLetExp :: at ) ->
            walk_exp exp at

        ( Lam arg exp, FLamArg :: at ) ->
            Nothing

        ( Lam arg exp, FLamExp :: at ) ->
            walk_exp exp at

        ( Case exp cases, FCaseExp :: at ) ->
            walk_exp exp at

        ( Case exp cases, (FCasePattern n) :: at ) ->
            cases !! n |> Maybe.map Tuple.first |> Maybe.andThen (\exp -> walk_exp exp at)

        ( Case exp cases, (FCaseResult n) :: at ) ->
            cases !! n |> Maybe.map Tuple.second |> Maybe.andThen (\exp -> walk_exp exp at)

        ( Tup exps, (FTup n) :: at ) ->
            exps !! n |> Maybe.andThen (\exp -> walk_exp exp at)

        ( exp, FPoint :: at ) ->
            Just exp

        _ ->
            Nothing


descend_right : Exp -> List Focus -> List Focus
descend_right exp_ acc =
    case exp_ of
        Apply fun arg ->
            descend_right arg (FApplyArg :: acc)

        Let bindings exp ->
            descend_right exp (FLetExp :: acc)

        Lam var exp ->
            descend_right exp (FLamExp :: acc)

        Tup exps ->
            case List.reverse exps of
                exp :: _ ->
                    descend_left exp (FTup (List.length exps - 1) :: acc)

                [] ->
                    List.reverse acc

        Case exp cases ->
            descend_right exp (FCaseExp :: acc)

        _ ->
            List.reverse acc


descend_left : Exp -> List Focus -> List Focus
descend_left exp_ acc =
    case exp_ of
        Apply fun arg ->
            descend_left fun (FApplyFun :: acc)

        Lam var exp ->
            List.reverse (FLamArg :: acc)

        Tup exps ->
            case exps of
                exp :: _ ->
                    descend_left exp (FTup 0 :: acc)

                [] ->
                    List.reverse acc

        Case exp cases ->
            descend_left exp (FCaseExp :: acc)

        _ ->
            List.reverse acc


ascend_left : Bindings -> List Focus -> List Focus
ascend_left bs at =
    case get_last at of
        Just focus ->
            case focus of
                FLetExp ->
                    update_last FLetBindings at []

                FLamArg ->
                    ascend_left bs (remove_last at [])

                FLamExp ->
                    update_last FLamArg at []

                FApplyFun ->
                    ascend_left bs (remove_last at [])

                FApplyArg ->
                    update_last FApplyFun at []

                FCaseExp ->
                    ascend_left bs (remove_last at [])

                FCasePattern n ->
                    ascend_left bs (remove_last at [])

                FCaseResult n ->
                    update_last (FCasePattern n) at []

                FRecordValue key ->
                    update_last (FRecordKey key) at []

                FTup n ->
                    if n > 0 then
                        update_last (FTup (n - 1)) at []
                    else
                        ascend_left bs (remove_last at [])

                FBindingValue n ->
                    update_last (FBindingName n) at []

                _ ->
                    ascend_left bs (remove_last at [])

        _ ->
            at


ascend_right : Bindings -> List Focus -> List Focus
ascend_right bs at =
    case get_last at of
        Just focus ->
            case focus of
                FLetBindings ->
                    update_last FLetExp at []

                FLamArg ->
                    update_last FLamExp at []

                FLamExp ->
                    ascend_right bs (remove_last at [])

                FApplyFun ->
                    update_last FApplyArg at []

                FApplyArg ->
                    ascend_right bs (remove_last at [])

                FCaseExp ->
                    ascend_right bs (remove_last at [])

                FCasePattern n ->
                    update_last (FCaseResult n) at []

                FRecordKey key ->
                    update_last (FRecordValue key) at []

                FTup n ->
                    case walk_bindings bs (remove_last at []) of
                        Just (Tup exps) ->
                            if (n + 1) < List.length exps then
                                update_last (FTup (n + 1)) at []
                            else
                                ascend_right bs (remove_last at [])

                        _ ->
                            ascend_right bs (remove_last at [])

                FBindingName n ->
                    update_last (FBindingValue n) at []

                _ ->
                    ascend_right bs (remove_last at [])

        _ ->
            at


ascend_up : Bindings -> List Focus -> List Focus
ascend_up bs at =
    case get_last at of
        Just focus ->
            case focus of
                FCasePattern n ->
                    if n > 0 then
                        update_last (FCasePattern (n - 1)) at []
                    else
                        update_last FCaseExp at []

                FCaseResult n ->
                    if n > 0 then
                        update_last (FCaseResult (n - 1)) at []
                    else
                        ascend_up bs (remove_last at [])

                FBindingName n ->
                    if n > 0 then
                        update_last (FBindingName (n - 1)) at []
                    else
                        ascend_up bs (remove_last at [])

                FBindingValue n ->
                    if n > 0 then
                        update_last (FBindingValue (n - 1)) at []
                    else
                        ascend_up bs (remove_last at [])

                _ ->
                    ascend_up bs (remove_last at [])

        _ ->
            at


ascend_down : Bindings -> List Focus -> List Focus
ascend_down bs at =
    case get_last at of
        Just focus ->
            case focus of
                FCaseExp ->
                    update_last (FCasePattern 0) at []

                FCasePattern n ->
                    case walk_bindings bs (remove_last at []) of
                        Just (Case exp cases) ->
                            if (n + 1) < List.length cases then
                                update_last (FCasePattern (n + 1)) at []
                            else
                                ascend_down bs (remove_last at [])

                        _ ->
                            ascend_down bs (remove_last at [])

                FCaseResult n ->
                    case walk_bindings bs (remove_last at []) of
                        Just (Case exp cases) ->
                            if (n + 1) < List.length cases then
                                update_last (FCaseResult (n + 1)) at []
                            else
                                ascend_down bs (remove_last at [])

                        _ ->
                            ascend_down bs (remove_last at [])

                FBindingName n ->
                    case walk_bindings bs (remove_last at []) of
                        Just (Record bindings) ->
                            if (n + 1) < DictList.length bindings then
                                update_last (FBindingName (n + 1)) at []
                            else
                                ascend_up bs (remove_last at [])

                        _ ->
                            ascend_down bs (remove_last at [])

                FBindingValue n ->
                    case walk_bindings bs (remove_last at []) of
                        Just (Record bindings) ->
                            if (n + 1) < DictList.length bindings then
                                update_last (FBindingValue (n + 1)) at []
                            else
                                ascend_up bs (remove_last at [])

                        _ ->
                            ascend_down bs (remove_last at [])

                _ ->
                    ascend_down bs (remove_last at [])

        _ ->
            at


type Layout
    = Horizontal
    | Vertical


type LayoutTree
    = Node Layout Exp (List LayoutTree)
    | Leaf Exp


to_layout_tree : Exp -> LayoutTree
to_layout_tree exp_ =
    case exp_ of
        Apply fun arg ->
            Node Horizontal exp_ [ to_layout_tree fun, to_layout_tree arg ]

        Let bindings exp ->
            Node Vertical exp_ [ to_layout_tree exp ]

        Lam arg exp ->
            Node Horizontal exp_ [ Leaf (Var ( Nothing, arg )), to_layout_tree exp ]

        Var name ->
            Leaf exp_

        Lit lit ->
            Leaf exp_

        Tup exps ->
            Node Horizontal exp_ (exps |> List.map (\exp -> to_layout_tree exp))

        Case exp cases ->
            Node Vertical exp_ []

        Record bindings ->
            Node Vertical exp_ []


to_layout : Exp -> Maybe Layout
to_layout exp_ =
    case exp_ of
        Apply fun arg ->
            Just Horizontal

        Let bindings exp ->
            Just Vertical

        Lam arg exp ->
            Just Horizontal

        Var name ->
            Nothing

        Lit lit ->
            Nothing

        Tup exps ->
            Just Horizontal

        Case exp cases ->
            Just Vertical

        Record bindings ->
            Just Vertical


key_press : Keyboard.Extra.Key -> Model -> Model
key_press key model =
    let
        ( ascend, descend, updown ) =
            Debug.log "ascended_cursor" <|
                case key of
                    ArrowLeft ->
                        ( ascend_left, descend_right, False )

                    ArrowRight ->
                        ( ascend_right, descend_left, False )

                    ArrowUp ->
                        ( ascend_up, descend_left, True )

                    ArrowDown ->
                        ( ascend_down, descend_left, True )

                    _ ->
                        ( always identity, always identity, False )

        ascended_cursor =
            ascend model.ast.bindings model.cursor

        tail =
            List.drop (List.length ascended_cursor - 1) model.cursor

        m_exp =
            Debug.log "m_exp" <| walk_bindings model.ast.bindings ascended_cursor

        cursor =
            Debug.log "cursor" <|
                case m_exp of
                    Just exp ->
                        case Debug.log "exps" <| ( updown, List.length tail > 1, walk_bindings model.ast.bindings ((remove_point ascended_cursor []) ++ tail) ) of
                            ( True, True, Just exp2 ) ->
                                (remove_point ((remove_point ascended_cursor []) ++ tail) []) ++ (descend exp2 []) ++ [ FPoint ]

                            _ ->
                                (remove_point ascended_cursor []) ++ (descend exp []) ++ [ FPoint ]

                    _ ->
                        ascended_cursor
    in
        { model
            | cursor =
                if cursor == [ FPoint ] then
                    model.cursor
                else
                    cursor
        }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Nop ->
            model

        ChangeSrc src ->
            { model | src = src, ast = translate (Ast.parse src) }

        OptionsMsg optsmsg ->
            { model | opts = update_opts optsmsg model.opts }

        ChangePointer pointer ->
            { model | pointer = pointer }

        ChangeCursor cursor ->
            { model | cursor = cursor }

        KeyPress key ->
            key_press key { model | keys_pressed = key :: model.keys_pressed }


main : Program { swapCount : Int } Model Msg
main =
    programWithFlags
        { init = \{ swapCount } -> ( init, Cmd.none )
        , update = \msg model -> ( update msg model, Cmd.none )
        , view = view
        , subscriptions = \_ -> Keyboard.Extra.downs KeyPress
        }
