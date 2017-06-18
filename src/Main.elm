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
            { opts | qualifiers = x }

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


ascend_left : List Focus -> List Focus
ascend_left at =
    case get_last at of
        Just focus ->
            case focus of
                FLetExp ->
                    update_last FLetBindings at []

                FLamArg ->
                    ascend_left (remove_last at [])

                FLamExp ->
                    update_last FLamArg at []

                FApplyFun ->
                    ascend_left (remove_last at [])

                FApplyArg ->
                    update_last FApplyFun at []

                FCasePattern n ->
                    ascend_left (remove_last at [])

                FCaseResult n ->
                    update_last (FCasePattern n) at []

                FRecordValue key ->
                    update_last (FRecordKey key) at []

                FTup n ->
                    if n > 0 then
                        update_last (FTup (n - 1)) at []
                    else
                        ascend_left (remove_last at [])

                _ ->
                    at

        _ ->
            at


ascend_right : List Focus -> List Focus
ascend_right at =
    case get_last at of
        Just focus ->
            case focus of
                FLetBindings ->
                    update_last FLetExp at []

                FLamArg ->
                    update_last FLamExp at []

                FLamExp ->
                    ascend_right (remove_last at [])

                FApplyFun ->
                    update_last FApplyArg at []

                FApplyArg ->
                    ascend_right (remove_last at [])

                FCasePattern n ->
                    update_last (FCaseResult n) at []

                FRecordKey key ->
                    update_last (FRecordValue key) at []

                FTup n ->
                    update_last (FTup (n + 1)) at []

                _ ->
                    at

        _ ->
            at


ascend_up : List Focus -> List Focus
ascend_up at =
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
                        ascend_up (remove_last at [])

                _ ->
                    ascend_up (remove_last at [])

        _ ->
            at


ascend_down : List Focus -> List Focus
ascend_down at =
    case get_last at of
        Just focus ->
            case focus of
                FCaseExp ->
                    update_last (FCasePattern 0) at []

                FCasePattern n ->
                    update_last (FCasePattern (n + 1)) at []

                FCaseResult n ->
                    update_last (FCaseResult (n + 1)) at []

                _ ->
                    ascend_down (remove_last at [])

        _ ->
            at


key_press : Keyboard.Extra.Key -> Model -> Model
key_press key model =
    let
        moved_cursor =
            Debug.log "moved_cursor" <|
                case key of
                    ArrowLeft ->
                        ascend_left model.cursor

                    ArrowRight ->
                        ascend_right model.cursor

                    ArrowUp ->
                        ascend_up model.cursor

                    ArrowDown ->
                        ascend_down model.cursor

                    _ ->
                        model.cursor

        descend =
            case key of
                ArrowLeft ->
                    descend_right

                ArrowRight ->
                    descend_left

                ArrowUp ->
                    descend_left

                ArrowDown ->
                    descend_left

                _ ->
                    \_ _ -> []

        m_exp =
            Debug.log "m_exp" <| walk_bindings model.ast.bindings moved_cursor

        cursor =
            Debug.log "cursor" <|
                case m_exp of
                    Just exp ->
                        (remove_point moved_cursor []) ++ (descend exp []) ++ [ FPoint ]

                    Nothing ->
                        moved_cursor
    in
        { model | cursor = cursor }


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
