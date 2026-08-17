(* js_of_ocaml entry point for the web demo. Exposes a small API on
   globalThis.crCalc. Evaluated terms are kept in a registry so that asking
   for more digits reuses the same term — and therefore its cached
   approximations — instead of rebuilding it. *)

open Base
open Js_of_ocaml

let table : (int, Constructive_reals.t) Hashtbl.t = Hashtbl.create (module Int)
let next_id = ref 0

let inject = Js.Unsafe.inject
let js_str s = inject (Js.string s)

let ok_result fields =
  Js.Unsafe.obj (Array.append [| ("ok", inject Js._true) |] fields)

let error_result msg =
  Js.Unsafe.obj [| ("ok", inject Js._false); ("error", js_str msg) |]

let exn_message = function
  | Failure m -> m
  | e -> Exn.to_string e

(* parse(exprString) -> {ok, id} | {ok: false, error}
   Parses and builds the constructive-real term (cheap and lazy — nothing is
   approximated yet). *)
let parse_js (s : Js.js_string Js.t) =
  try
    let expr = Calc_core.parse (Js.to_string s) in
    let cr = Calc_core.Evaluator.eval expr in
    let id = !next_id in
    Int.incr next_id;
    Hashtbl.set table ~key:id ~data:cr;
    ok_result [| ("id", inject id) |]
  with e -> error_result (exn_message e)

(* evalCr(id, digits) -> {ok, value} — forces the term to `digits` decimal
   places. Every digit shown is correct. *)
let eval_cr (id : int) (digits : int) =
  match Hashtbl.find table id with
  | None -> error_result "unknown expression id"
  | Some cr -> (
      try
        let value =
          Constructive_reals.eval_to_string ~digits:(Int32.of_int_exn digits) cr
        in
        ok_result [| ("value", js_str value) |]
      with e -> error_result (exn_message e))

(* evalFloat(exprString) -> {ok, value} — the same AST evaluated with 64-bit
   floats (i.e. JavaScript numbers), shown shortest-round-trip like JS does. *)
let eval_float (s : Js.js_string Js.t) =
  try
    let expr = Calc_core.parse (Js.to_string s) in
    let f = Calc_core.Float_evaluator.eval expr in
    ok_result [| ("value", js_str (Calc_core.Float_evaluator.shortest_string f)) |]
  with e -> error_result (exn_message e)

(* repr(id) -> {ok, pp, debug} — the term's structure (pretty-printed
   expression) and its internal state including cached approximations. *)
let repr (id : int) =
  match Hashtbl.find table id with
  | None -> error_result "unknown expression id"
  | Some cr ->
      ok_result
        [| ("pp", js_str (Fmt.str "%a" Constructive_reals.pp cr))
         ; ("debug", js_str (Constructive_reals.debug_to_string cr))
        |]

let () =
  Js.export "crCalc"
    (Js.Unsafe.obj
       [| ("parse", inject (Js.wrap_callback parse_js))
        ; ("evalCr", inject (Js.wrap_callback eval_cr))
        ; ("evalFloat", inject (Js.wrap_callback eval_float))
        ; ("repr", inject (Js.wrap_callback repr))
       |])
