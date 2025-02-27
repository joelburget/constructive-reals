open Base
open Stdio
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
        | _ ->
            printf "Unexpected character at position %d\n" i;
            loop next_i acc
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

    let result, _ = expr tokens in
    result
end

module Evaluator = struct
  open Parser

  let eval expr =
    let rec eval_expr = function
      | Num n -> (
          try of_float (Float.of_string n)
          with _ -> failwith ("Invalid number: " ^ n))
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

let rec repl () =
  printf "> ";
  Out_channel.flush stdout;
  match In_channel.input_line stdin with
  | None -> ()
  | Some "quit" | Some "exit" -> ()
  | Some line ->
      (try
         let tokens = Lexer.tokenize line in
         let expr = Parser.parse_expr tokens in
         let result = Evaluator.eval expr in
         printf "%s\n" (eval_to_string result)
       with e -> printf "Error: %s\n" (Exn.to_string e));
      repl ()

let () =
  printf "Constructive Reals Calculator\n";
  printf "Type expressions using +, -, *, /, sin, cos, etc.\n";
  printf
    "Available functions: sin, cos, tan, asin, acos, atan, exp, ln, sqrt, abs, \
     max, min\n";
  printf "Constants: pi, e\n";
  printf "Type 'quit' or 'exit' to exit\n\n";
  repl ()
