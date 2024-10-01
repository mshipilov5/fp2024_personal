[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Base
open Ast


let list_remove x = List.filter ~f:(fun a -> not (String.equal a x))


(**let rec free_vars = function
  | Evar s -> [s]
  | Efun (args, body) -> 
      let acc = List.fold_right ~f:list_remove ~init:[] args in
      list_remove s (free_vars body)
  | Efun_application (l, r) -> 
      List.append (free_vars l) (free_vars r)
  | Elet (name, value, body) -> 
      let free_in_value = free_vars value in
      let free_in_body = list_remove name (free_vars body) in
      List.append free_in_value free_in_body
  | Elet_rec (name, value, body) ->
      let free_in_value = free_vars value in
      let free_in_body = list_remove name (free_vars body) in
      List.append free_in_value free_in_body
  | Eif_then_else (cond, then_expr, else_expr) ->
      let free_in_cond = free_vars cond in
      let free_in_then = free_vars then_expr in
      let free_in_else = Option.value_map else_expr ~default:[] ~f:free_vars in
      List.append (List.append free_in_cond free_in_then) free_in_else
  | Etuple exprs | Elist exprs ->
      List.concat_map exprs ~f:free_vars
  | Ebin_op (_, e1, e2) ->
      List.append (free_vars e1) (free_vars e2)
  | Eun_op (_, e) ->
      free_vars e
  | Econst _ -> [] 
  

let is_free_in x term = List.mem (free_vars term) x ~equal:String.equal**)


let var x = Evar x
let abs x l = Efun ([x], l)
let app l r = Efun_application (l, r)


module type MONAD_FAIL = sig
  include Base.Monad.S2

  val fail : 'e -> ('a, 'e) t
end


module ResultMonad : MONAD_FAIL with type 'a t = ('a, 'e) Result.t = struct
  include Result

  let fail e = Error e
end