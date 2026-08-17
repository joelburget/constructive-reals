(* Expression language shared by the terminal calculator and the web demo:
   lexer, parser, and two evaluators over the same AST — one producing
   constructive reals, one producing ordinary 64-bit floats for comparison. *)

open Base
open Constructive_reals

module Lexer = struct
  type token =
    | PLUS
    | MINUS
    | TIMES
    | DIV
    | LPAREN
    | RPAREN
    | COMMA
    | NUMBER of string
    | IDENTIFIER of string
    | EOF

  let tokenize (input : string) : token list =
    let len = String.length input in
    let rec loop (i : int) (acc : token list) : token list =
      if i >= len then List.rev (EOF :: acc)
      else
        let next_i = Int.(i + 1) in
        match input.[i] with
        | ' ' | '\t' | '\n' -> loop next_i acc
        | '+' -> loop next_i (PLUS :: acc)
        | '-' -> loop next_i (MINUS :: acc)
        | '*' -> loop next_i (TIMES :: acc)
        | '/' -> loop next_i (DIV :: acc)
        | '(' -> loop next_i (LPAREN :: acc)
        | ')' -> loop next_i (RPAREN :: acc)
        | ',' -> loop next_i (COMMA :: acc)
        | c when Char.(is_digit c || c = '.') ->
            let j = ref i in
            while !j < len && Char.(is_digit input.[!j] || input.[!j] = '.') do
              Int.incr j
            done;
            let number = String.sub input ~pos:i ~len:Int.(!j - i) in
            loop !j (NUMBER number :: acc)
        | c when Char.is_alpha c ->
            let j = ref i in
            while
              !j < len && Char.(is_alpha input.[!j] || is_digit input.[!j])
            do
              Int.incr j
            done;
            let id = String.sub input ~pos:i ~len:Int.(!j - i) in
            loop !j (IDENTIFIER id :: acc)
        | c -> failwith (Printf.sprintf "Unexpected character '%c' at position %d" c i)
    in
    loop 0 []
end

module Parser = struct
  open Lexer

  type expr =
    | Num of string
    | Var of string
    | Add of expr * expr
    | Sub of expr * expr
    | Mul of expr * expr
    | Div of expr * expr
    | Neg of expr
    | Apply of string * expr list

  (* Recursive descent parser *)
  let parse_expr tokens =
    let rec expr tokens =
      let t, tokens' = term tokens in
      expr_tail t tokens'
    and expr_tail left tokens =
      match tokens with
      | PLUS :: rest ->
          let right, rest' = term rest in
          expr_tail (Add (left, right)) rest'
      | MINUS :: rest ->
          let right, rest' = term rest in
          expr_tail (Sub (left, right)) rest'
      | _ -> (left, tokens)
    and term tokens =
      let f, tokens' = factor tokens in
      term_tail f tokens'
    and term_tail left tokens =
      match tokens with
      | TIMES :: rest ->
          let right, rest' = factor rest in
          term_tail (Mul (left, right)) rest'
      | DIV :: rest ->
          let right, rest' = factor rest in
          term_tail (Div (left, right)) rest'
      | _ -> (left, tokens)
    and factor = function
      | NUMBER n :: rest -> (Num n, rest)
      | IDENTIFIER "pi" :: rest -> (Var "pi", rest)
      | IDENTIFIER "e" :: rest -> (Var "e", rest)
      | IDENTIFIER name :: LPAREN :: rest ->
          let args, rest' = parse_args rest in
          (Apply (name, args), rest')
      | IDENTIFIER name :: rest -> (Var name, rest)
      | LPAREN :: rest -> (
          let e, rest' = expr rest in
          match rest' with
          | RPAREN :: rest'' -> (e, rest'')
          | _ -> failwith "Expected closing parenthesis")
      | MINUS :: rest ->
          let e, rest' = factor rest in
          (Neg e, rest')
      | _ -> failwith "Unexpected token in factor"
    and parse_args tokens =
      match tokens with
      | RPAREN :: rest -> ([], rest)
      | _ -> (
          let arg, rest = expr tokens in
          match rest with
          | RPAREN :: rest' -> ([ arg ], rest')
          | COMMA :: rest' ->
              let args, rest'' = parse_args rest' in
              (arg :: args, rest'')
          | EOF :: _ -> failwith "Unexpected end of input in function arguments"
          | _ ->
              failwith
                "Expected comma or closing parenthesis after function arguments"
          )
    in

    let result, rest = expr tokens in
    (match rest with
     | [] | [ EOF ] -> ()
     | _ -> failwith "Unexpected trailing input");
    result
end

let parse (input : string) : Parser.expr =
  Parser.parse_expr (Lexer.tokenize input)

module Evaluator = struct
  open Parser

  (* Parse a decimal literal exactly: "123.45" is 12345/100, not the nearest
     float. Rounding through a float here would make the calculator inherit
     exactly the errors it's meant to avoid. *)
  let of_decimal_string (s : string) : Constructive_reals.t =
    let exact whole frac =
      let digits = of_bigint (Z.of_string ("0" ^ whole ^ frac)) in
      if String.is_empty frac then digits
      else digits / of_bigint (Z.pow (Z.of_int 10) (String.length frac))
    in
    try
      match String.split s ~on:'.' with
      | [ whole ] -> exact whole ""
      | [ ""; "" ] -> failwith "empty"
      | [ whole; frac ] -> exact whole frac
      | _ -> failwith "too many dots"
    with _ -> failwith ("Invalid number: " ^ s)

  let eval expr =
    let rec eval_expr = function
      | Num n -> of_decimal_string n
      | Var "pi" -> pi
      | Var "e" -> e
      | Var name -> failwith ("Unknown variable: " ^ name)
      | Add (e1, e2) -> eval_expr e1 + eval_expr e2
      | Sub (e1, e2) -> eval_expr e1 - eval_expr e2
      | Mul (e1, e2) -> eval_expr e1 * eval_expr e2
      | Div (e1, e2) -> eval_expr e1 / eval_expr e2
      | Neg e -> negate (eval_expr e)
      | Apply (fn, args) -> (
          match (fn, args) with
          | "sin", [ e ] -> sin (eval_expr e)
          | "cos", [ e ] -> cos (eval_expr e)
          | "tan", [ e ] -> tan (eval_expr e)
          | "asin", [ e ] -> asin (eval_expr e)
          | "acos", [ e ] -> acos (eval_expr e)
          | "atan", [ e ] -> atan (eval_expr e)
          | "exp", [ e ] -> exp (eval_expr e)
          | "ln", [ e ] -> ln (eval_expr e)
          | "sqrt", [ e ] -> sqrt (eval_expr e)
          | "abs", [ e ] -> abs (eval_expr e)
          | "max", [ e1; e2 ] -> max (eval_expr e1) (eval_expr e2)
          | "min", [ e1; e2 ] -> min (eval_expr e1) (eval_expr e2)
          | _ -> failwith ("Unknown function: " ^ fn))
    in
    eval_expr expr
end

(* The same AST evaluated with ordinary IEEE 754 doubles — under js_of_ocaml
   these are exactly JavaScript numbers, so this is "what JavaScript says". *)
module Float_evaluator = struct
  open Parser

  let eval expr =
    let rec eval_expr = function
      | Num n -> Float.of_string n
      | Var "pi" -> Float.pi
      | Var "e" -> Float.exp 1.0
      | Var name -> failwith ("Unknown variable: " ^ name)
      | Add (e1, e2) -> eval_expr e1 +. eval_expr e2
      | Sub (e1, e2) -> eval_expr e1 -. eval_expr e2
      | Mul (e1, e2) -> eval_expr e1 *. eval_expr e2
      | Div (e1, e2) -> eval_expr e1 /. eval_expr e2
      | Neg e -> Float.neg (eval_expr e)
      | Apply (fn, args) -> (
          match (fn, args) with
          | "sin", [ e ] -> Float.sin (eval_expr e)
          | "cos", [ e ] -> Float.cos (eval_expr e)
          | "tan", [ e ] -> Float.tan (eval_expr e)
          | "asin", [ e ] -> Float.asin (eval_expr e)
          | "acos", [ e ] -> Float.acos (eval_expr e)
          | "atan", [ e ] -> Float.atan (eval_expr e)
          | "exp", [ e ] -> Float.exp (eval_expr e)
          | "ln", [ e ] -> Float.log (eval_expr e)
          | "sqrt", [ e ] -> Float.sqrt (eval_expr e)
          | "abs", [ e ] -> Float.abs (eval_expr e)
          | "max", [ e1; e2 ] -> Float.max (eval_expr e1) (eval_expr e2)
          | "min", [ e1; e2 ] -> Float.min (eval_expr e1) (eval_expr e2)
          | _ -> failwith ("Unknown function: " ^ fn))
    in
    eval_expr expr

  (* Shortest decimal string that round-trips to the same double — the same
     rule JavaScript uses to display numbers. *)
  let shortest_string (f : float) : string =
    if Float.is_nan f then "NaN"
    else if Float.is_inf f then (if Float.(f > 0.0) then "Infinity" else "-Infinity")
    else begin
      let result = ref (Printf.sprintf "%.17g" f) in
      (try
         for p = 1 to 17 do
           let s = Printf.sprintf "%.*g" p f in
           if Float.equal (Float.of_string s) f then begin
             result := s;
             Stdlib.raise_notrace Stdlib.Exit
           end
         done
       with Stdlib.Exit -> ());
      (* Normalize exponents to JavaScript style: e-05 -> e-5 *)
      let re_zeros s =
        match String.lsplit2 s ~on:'e' with
        | None -> s
        | Some (mantissa, ex) ->
          let sign, digits =
            match String.to_list ex with
            | '+' :: rest -> ("+", rest)
            | '-' :: rest -> ("-", rest)
            | rest -> ("+", rest)
          in
          let digits = List.drop_while digits ~f:(Char.equal '0') in
          let digits = if List.is_empty digits then [ '0' ] else digits in
          mantissa ^ "e" ^ sign ^ String.of_char_list digits
      in
      re_zeros !result
    end
end
