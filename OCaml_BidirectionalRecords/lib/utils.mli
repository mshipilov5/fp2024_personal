[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]


val list_remove : string -> string list -> string list
val free_vars : Ast.expr -> string list
val is_free_in : string -> Ast.expr -> bool
val var : string -> Ast.expr
val abs : string -> Ast.expr -> Ast.expr
val app : Ast.expr -> Ast.expr -> Ast.expr


module type MONAD_FAIL = sig
  include Base.Monad.S2

  val fail : 'e -> ('a, 'e) t
end
