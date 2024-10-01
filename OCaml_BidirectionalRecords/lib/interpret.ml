[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Utils
open Ast
open Angstrom
open Base

type value =
  | VInt of int
  | VBool of bool
  | VChar of char
  | VString of string
  | VUnit
  | VTuple of value list
  | VList of value list
  | VFun of pattern * expr * binding

and binding = (id, value, Base.String.comparator_witness) Base.Map.t

type error =
  | UnknownVariable of string
  | DivisionByZero
  | TypeError of string
  | UnboundValue of string


  module type Monad = sig
    type ('a, 'e) t
  
    val return : 'a -> ('a, 'e) t
    val fail : 'e -> ('a, 'e) t
    val ( let* ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
  end
  
  module Env (M : Monad) = struct
    open M
  
    let empty = Base.Map.empty (module Base.String)
    let extend key value env = Base.Map.update env key ~f:(fun _ -> value)
  
    let find map key =
      match Base.Map.find map key with
      | Some value -> return value
      | None -> fail (UnboundValue key)
    ;;
  end
  
  module Interpret (M : Monad) = struct
    open M
    open Env (M)


  type environment = (id * const) list


  let eval_bin_op op v1 v2 =
    match op, v1, v2 with
    | Add, VInt x, VInt y -> M.return (VInt (x + y))
    | Sub, VInt x, VInt y -> M.return (VInt (x - y))
    | Mult, VInt x, VInt y -> M.return (VInt (x * y))
    | Div, VInt x, VInt 0 -> M.fail DivisionByZero
    | Div, VInt x, VInt y -> M.return (VInt (x / y))
    | Gt, VInt x, VInt y -> M.return (VBool (x > y))
    | Lt, VInt x, VInt y -> M.return (VBool (x < y))
    | Eq, VInt x, VInt y -> M.return (VBool (x = y))
    | Neq, VInt x, VInt y -> M.return (VBool (x <> y))
    | Gte, VInt x, VInt y -> M.return (VBool (x >= y))
    | Lte, VInt x, VInt y -> M.return (VBool (x <= y))
    | And, VBool x, VBool y -> M.return (VBool (x && y))
    | Or, VBool x, VBool y -> M.return (VBool (x || y))
    | _ -> M.fail (TypeError "Unsupported binary operation or mismatched types\n%!")


  let eval_un_op op v =
    match op, v with
    | Negative, VInt x -> M.return (VInt (-x))
    | Not, VBool b -> M.return (VBool (not b))
    | _ -> M.fail (TypeError "Unsupported unary operation or mismatched types\n%!")


    let lookup env x =
      let* v = find env x in
      match v with
      | VFun (p, e, env) -> return (VFun (p, e, extend x v env))
      | VInt n -> return (VInt n)
      | VBool b -> return (VBool b)
      | _ -> fail (TypeError "Unsupported value type\n%!")
    ;;


  let rec eval env = function
    | Econst c -> M.return c
    | Evar v -> lookup env v >>= fun v_value -> (* Convert v_value to const if necessary *)
      match v_value with
      | Int n -> M.return (Int n)  (* If v_value is Int, handle accordingly *)
      | Bool b -> M.return (Bool b)
      | _ -> fail (TypeError "Expected const\n%!")
    | Ebin_op (op, e1, e2) ->
      eval env e1 >>= fun v1 ->
      eval env e2 >>= fun v2 ->
      eval_bin_op op v1 v2
    | Eun_op (op, e) ->
      eval env e >>= fun v -> eval_un_op op v
    | Eif_then_else (cond, e1, e2_opt) ->
      eval env cond >>= (function
        | VBool true -> eval env e1
        | VBool false -> (match e2_opt with Some e2 -> eval env e2 | None -> M.return VUnit)
        | _ -> M.fail (TypeError "Condition should evaluate to a boolean\n%!"))
    | Elet (flag, id, e1, e2) ->
        eval env e1 >>= fun v1 ->
        (match flag with
        | NonRec -> 
            let new_env = (id, v1) :: env in
            eval new_env e2
        | Rec ->
            let rec_env = (id, v1) :: env in
            eval rec_env e2)
    | Efun (params, body) -> M.return (Efun (params, body))
    | Efun_application (Efun (params, body), arg) ->
      eval env arg >>= fun v_arg ->
      let new_env = List.combine params [v_arg] @ env in
      eval new_env body >>= function
      | VInt result -> M.return result
      | _ -> M.fail (TypeError "Function should return an integer\n%!")
    | _ -> M.fail (TypeError "Unsupported expression type\n%!")


  let run ast =
    eval [] ast >>= function
    | VInt n -> M.return n
    | _ -> M.fail (TypeError "Top-level expression should evaluate to an integer\n%!")
  ;;
end


let parse_and_run str =
  let module I = Interpret (Base.Result) in
  let rez = Base.Result.(Parser.parse str >>= I.run) in
  match rez with
  | Result.Ok n -> Printf.printf "Success: %d\n" n
  | Result.Error #Parser.error ->
    Format.eprintf "Parsing error\n%!";
    exit 1
  | Result.Error #error as err ->
    (match err with
    | Result.Error (UnknownVariable var) -> Format.eprintf "Unknown variable: %s\n%!" var
    | Result.Error DivisionByZero -> Format.eprintf "Error: Division by zero\n%!"
    | Result.Error (TypeError msg) -> Format.eprintf "Type error: %s\n%!" msg
    | _ -> Format.eprintf "Interpreter error\n%!");
    exit 1
;;
