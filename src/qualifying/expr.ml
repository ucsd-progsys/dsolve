open Asttypes
open Longident
open Parsetree
open Predicate

module ExpMap = Map.Make(Expression)

exception ExpressionNotSupported

let expr_builtin_qualifier exp =
  match exp.pexp_desc with
    | Pexp_constant(Const_int n) ->
        Some(Builtins.equality_qualifier (PInt n))
    | Pexp_ident(Lident x) ->
        if Env.mem x Builtins.types then
          None
        else
          Some(Builtins.equality_qualifier (Var x))
    | _ -> None
