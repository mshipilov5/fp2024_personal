[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Lambda_lib

let () =
  let code = "
    let rec fact = fun x -> if x <= 1 then 1 else x * fact (x - 1) in fact 5
  " in
  Interpret.parse_and_run code
