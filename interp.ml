let fact_test : string =
"(((lambda (g) (g g))
   (lambda (f)
     (lambda (n)
       (if0 n 1 ( * n ((f f) (+ n -1)))))))
  10)"

(* Because of silent overflow, values much larger than 10 factorial are not recommended! *)


include List;;
include String;;
# load "str.cma";;  (* namespace: Str *)

(* Lisp s-expressions *)
type sexpr = SExprNum of int
           | SExprSym of string
           | SExprList of sexpr list;;

(* Internal representation of expressions *)
type expr = ExprNum of int
          | ExprId of string
          | ExprAdd of expr * expr
          | ExprMult of expr * expr
          | ExprIf0 of expr * expr * expr
          | ExprLambda of string * expr
          | ExprApp of expr * expr;;

type value = ValNum of int
           | ValClosure of string * expr * environment

and environment = (string * value) list;;



(* STEP 1: READ strings as s-expressions *)

let tokenize (code : string) : string list =
  let paddedl = Str.global_replace (Str.regexp "(") " ( " code
  in let paddedr = Str.global_replace (Str.regexp ")") " ) " paddedl
     in  Str.split (Str.regexp "[ \t\n]+") paddedr;;

(* is_num: Test wheter a string represents a (positive or negative) integer,
   before trying to cast it. TODO: maybe there's a builtin I missed? *)
let rec is_num (s : string) : bool = 
  let rec n = length s
  and only_digits index =
    if index >= n then true
    else if contains "0123456789" (s.[index]) then only_digits (index + 1)
    else false
  in if n = 0 then false
     else if s.[0] = '-' then only_digits 1
     else only_digits 0;;

(* read_one and read_star take strings off the stack of "tokens" and build
   s-expressions. In functional style, the stack is passed as a parameter
   and return value rather than updated destructively (even though Lisp code
   can be read without backtracking). *)
let rec read_one (tokens : string list) : sexpr * string list =
  match tokens with
  | (head::tail) -> ( match head with
    | _ when is_num head -> (SExprNum (int_of_string head), tail)
    | "(" -> read_star tail
    | ")" -> failwith "read error: unexpected right paren in read_one"
    | _ -> (SExprSym head, tail) )
  | [] -> failwith "read error: empty list to read_one"

and read_star (tokens : string list) : sexpr * string list = 
  let rec helper tokens s_list = 
    match tokens with
    | [] -> failwith "read error: list not terminated in read_star"
    | (")"::tail) -> (s_list, tail)
    | _ -> ( match read_one tokens with
      | (s, rest_tokens) -> helper rest_tokens (s::s_list) )
  in match helper tokens [] with
     | (s_list, rest_tokens) -> (SExprList (rev s_list), rest_tokens);;



(* STEP 2: PARSE s-expressions to internal expr representation *)

let rec parse (s : sexpr) : expr =
  match s with
  | SExprNum n -> ExprNum n
  | SExprSym sym -> ExprId sym
  | SExprList [] -> failwith "parse error: empty s-expression"
  | SExprList (head::tail) -> ( match head with
    | SExprSym "+" -> ( match tail with
      | [left; right] -> ExprAdd (parse left, parse right)
      | _ -> failwith "parse error: + takes exactly 2 arguments" )
    | SExprSym "*" -> ( match tail with
      | [left; right] -> ExprMult (parse left, parse right)
      | _ -> failwith "parse error: * takes exactly 2 arguments" )
    | SExprSym "if0" -> ( match tail with
      | [pred; con; alt] -> ExprIf0 (parse pred, parse con, parse alt)
      | _ -> failwith "parse error: bad if0 expression" )
    | SExprSym "lambda" -> ( match tail with
      | [SExprList [SExprSym var]; body] -> ExprLambda (var, parse body)
      | _ -> failwith "parse error: bad lambda expression" )
    | _ -> ( match tail with
      | [arg] -> ExprApp (parse head, parse arg)
      | _ -> failwith "parse error: bad application expression" ) );;



(* STEP 3: EVAL expressions to values *)

(* assoc: if key is present in an association list, return the tail of that
   list, starting with the first place where the key matches. Cf. the Lisp
   builtin of same name. *)
let rec assoc (key : 'a) (lst : ('a * 'b) list) : ('a * 'b) list =
  match (lst, key) with
  | ([], _) -> []
  | ((heada, headb)::tail, _) when heada = key -> lst
  | (_::tail, _) -> assoc key tail;;

let env_lookup (var : string) (env : environment) : value =
  match (assoc var env) with
  | (_, value)::tail -> value
  | [] -> failwith ("Unbound identifier: " ^ var);;

(* add_val and mult_val steal + and * from the underlying OCaml, and apply
   them to values *)
let add_val (left : value) (right : value) : value =
  match (left, right) with
  | (ValNum x, ValNum y) -> ValNum (x+y)
  | _ -> failwith "Non-numeric values to +";;
  
let mult_val (left : value) (right : value) : value =
  match (left, right) with
  | (ValNum x, ValNum y) -> ValNum (x*y)
  | _ -> failwith "Non-numeric values to *";;
  
let rec eval (expr : expr) (env : environment) : value =
  match expr with
  | ExprNum num -> ValNum num
  | ExprId var -> env_lookup var env
  | ExprAdd (left, right) -> add_val (eval left env) (eval right env)
  | ExprMult (left, right) -> mult_val (eval left env) (eval right env)
  | ExprIf0 (test, conseq, alt) ->
    (match (eval test env) with
     | ValNum 0 -> eval conseq env
     | _ -> eval alt env)
  | ExprLambda (var, body) -> ValClosure (var, body, env)
  | ExprApp (proc, arg) ->
    (match (eval proc env) with
     | ValClosure (var, body, env1) -> eval body ((var, eval arg env)::env1)
     | _ -> failwith "Can only apply procedures");;



(* STEP 4: PRINT values as text *)

let show (value : value) : string =
  match value with
  | ValNum x -> string_of_int x
  | ValClosure (var,body,env) -> "#<procedure>"

let run (code : string) : unit =
  match read_one (tokenize code) with
    | (sexpr, _) -> print_endline (show (eval (parse sexpr) []));;


(* TESTING *)

run fact_test;;

