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

(* Size of the term if printed as a tree, i.e. with shared subterms expanded.
   Saturates at limit+1: terms like exp(1000) are built by repeated squaring
   of shared nodes, so their expanded size is exponential (2^depth) even
   though the DAG is tiny — printing them would build gigabyte strings. *)
let expanded_size (t : Constructive_reals.t) ~(limit : int) : int =
  let seen : (Constructive_reals.t * int) list ref = ref [] in
  let rec go t =
    match List.find !seen ~f:(fun (t', _) -> phys_equal t t') with
    | Some (_, s) -> s
    | None ->
        let children =
          match Constructive_reals.view t with
          | IntV _ | PiV -> []
          | AssumedIntV a | ShiftedV (a, _) | NegV a | InvV a
          | PrescaledExpV a | PrescaledCosV a | PrescaledLnV a
          | PrescaledAsinV a | SqrtV a -> [ a ]
          | AddV (a, b) | MultV (a, b) -> [ a; b ]
          | SelectV (s, a, b) -> [ s; a; b ]
        in
        let s =
          List.fold children ~init:1 ~f:(fun acc c ->
              Int.min (acc + go c) (limit + 1))
        in
        seen := (t, s) :: !seen;
        s
  in
  go t

let pp_limit = 2000

(* repr(id) -> {ok, pp} — the term pretty-printed as an expression, when
   that's printable at all. *)
let repr (id : int) =
  match Hashtbl.find table id with
  | None -> error_result "unknown expression id"
  | Some cr ->
      let pp_str =
        if expanded_size cr ~limit:pp_limit <= pp_limit
        then Fmt.str "%a" Constructive_reals.pp cr
        else
          "(too large to pretty-print — the term is a small graph with heavy \
           sharing; see the DAG below)"
      in
      ok_result [| ("pp", js_str pp_str) |]

(* A cached approximation (max_appr, min_prec) as a double, i.e. roughly
   max_appr * 2^min_prec. max_appr can be far larger than a double, so
   pre-shift it down and fold the shift into the exponent. *)
let approx_float (max_appr : Z.t) (min_prec : int32) : float =
  let numbits = Z.numbits max_appr in
  if numbits <= 512
  then Float.ldexp (Z.to_float max_appr) (Int32.to_int_exn min_prec)
  else begin
    let shift = numbits - 64 in
    let m = Z.to_float (Z.shift_right max_appr shift) in
    Float.ldexp m (Int32.to_int_exn min_prec + shift)
  end

(* dag(id) -> {ok, root, nodes} — the term graph. nodes[i] is
   {id, op, arg?, valid, maxAppr?, minPrec?, approx?, children: [ids]}.
   Nodes reachable more than once (shared subterms) appear once, so this is
   a true DAG encoding. *)
let dag (id : int) =
  match Hashtbl.find table id with
  | None -> error_result "unknown expression id"
  | Some cr ->
      let seen : (Constructive_reals.t * int) list ref = ref [] in
      let objs : (int * Js.Unsafe.any) list ref = ref [] in
      let counter = ref 0 in
      let rec go (t : Constructive_reals.t) : int =
        match
          List.find !seen ~f:(fun (t', _) -> phys_equal t t')
        with
        | Some (_, i) -> i
        | None ->
            let i = !counter in
            Int.incr counter;
            seen := (t, i) :: !seen;
            let op, arg, children =
              match Constructive_reals.view t with
              | IntV z -> ("int", Some (Z.to_string z), [])
              | AssumedIntV a -> ("assumed_int", None, [ a ])
              | AddV (a, b) -> ("add", None, [ a; b ])
              | ShiftedV (a, k) -> ("shift", Some (Int32.to_string k), [ a ])
              | NegV a -> ("neg", None, [ a ])
              | SelectV (s, a, b) -> ("select", None, [ s; a; b ])
              | MultV (a, b) -> ("mult", None, [ a; b ])
              | InvV a -> ("inv", None, [ a ])
              | PrescaledExpV a -> ("exp", None, [ a ])
              | PrescaledCosV a -> ("cos", None, [ a ])
              | PrescaledLnV a -> ("ln", None, [ a ])
              | PrescaledAsinV a -> ("asin", None, [ a ])
              | SqrtV a -> ("sqrt", None, [ a ])
              | PiV -> ("pi", None, [])
            in
            let child_ids = List.map children ~f:go in
            let fields =
              [ Some ("id", inject i)
              ; Some ("op", js_str op)
              ; Option.map arg ~f:(fun a -> ("arg", js_str a))
              ; Some
                  ( "children"
                  , inject (Js.array (Array.of_list child_ids)) )
              ]
            in
            let appr_fields =
              match Constructive_reals.approximation t with
              | None -> [ Some ("valid", inject Js._false) ]
              | Some (max_appr, min_prec) ->
                  [ Some ("valid", inject Js._true)
                  ; Some ("maxAppr", js_str (Z.to_string max_appr))
                  ; Some ("minPrec", inject (Int32.to_int_exn min_prec))
                  ; Some
                      ( "approx"
                      , js_str
                          (Calc_core.Float_evaluator.shortest_string
                             (approx_float max_appr min_prec)) )
                  ]
            in
            let obj =
              Js.Unsafe.obj
                (Array.of_list (List.filter_opt (fields @ appr_fields)))
            in
            objs := (i, obj) :: !objs;
            i
      in
      let root = go cr in
      let arr = Array.create ~len:!counter (inject Js.null) in
      List.iter !objs ~f:(fun (i, o) -> arr.(i) <- o);
      ok_result [| ("root", inject root); ("nodes", inject (Js.array arr)) |]

let () =
  Js.export "crCalc"
    (Js.Unsafe.obj
       [| ("parse", inject (Js.wrap_callback parse_js))
        ; ("evalCr", inject (Js.wrap_callback eval_cr))
        ; ("evalFloat", inject (Js.wrap_callback eval_float))
        ; ("repr", inject (Js.wrap_callback repr))
        ; ("dag", inject (Js.wrap_callback dag))
       |])
