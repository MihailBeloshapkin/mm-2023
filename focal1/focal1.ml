open Angstrom
open Str
open Caml.Format

type number = int * int

type ident = string

type value = 
  | Int of int
  | Float of float
  | String of string

type binop =
  | Add
  | Sub
  | Mul
  | Div 

type expr =
  | Set of ident * expr
  | Arythm of binop * expr * expr
  | Do of number
  | Goto of number
  | Ident of ident
  | Return
  | Type of expr
  | If of expr * number * number * number
  | For of ident * int * int * int * expr
  | Literal of value
type line = number * expr
 

(** Parsers  *)
let is_whitespace = function
  | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
  | _ -> false
;;

let is_small_letter = function
| 'a' .. 'z' -> true
| _ -> false
;;

let is_big_letter = function
| 'A' .. 'Z' -> true
| _ -> false
;;

let is_letter l = is_small_letter l || is_big_letter l

let is_digit = function
| '0' .. '9' -> true
| _ -> false
;;

let space = take_while is_whitespace
let space1 = take_while1 is_whitespace
let token s = space *> string s

let is_keyword _ = false

let new_ident =
  space *> take_while1 is_small_letter
  >>= fun str ->
  if is_keyword str then fail "Keyword in the wrong place of program" else return str
;;

let number_parser =
  space *> take_while1 is_digit
  >>= fun first_num ->
  char '.' *> take_while1 is_digit
  >>= fun snd_num -> return (int_of_string first_num, int_of_string snd_num)
;;

let int_parser = space *> take_while1 is_digit

let int_token =
  space *> take_while1 is_digit 
  >>= fun res -> return @@ Literal (Int (int_of_string res))
;;

let token_binop_parse = 
  space *> choice
    [ 
      token "+";
      token "-";
      token "*";
      token "/"
    ]

type dispatch = { e : dispatch -> expr t }

let type_d =  
  let set_parser d =
    fix 
    @@ fun _ ->  
    lift2 
      (fun i e -> Set (i, e))
      (token "SET" *> space1 *> new_ident <* token "=")
      (d.e d)
  in
  let return_parser _ =
    space *> token "RETURN"
    >>= fun _ -> return Return
  in
  let if_parser d =
    fix
    @@ fun _ ->
    lift4
      (fun a b c d -> If (a, b, c, d))
      (token "IF" *> space1 *> char '(' *> d.e d <* char ')')
      (space1 *> number_parser)
      (char ',' *> number_parser)
      (char ',' *> number_parser)
  in
  let for_parser d =
    fix 
    @@ fun _ ->
    lift3
      (fun id (a, b, c) cmd -> For (id, a, b, c, cmd))
      (token "FOR" *> space1 *> new_ident <* token "=")
      (lift3 
        (fun a b c -> int_of_string a, int_of_string b, int_of_string c) (int_parser <* char ',') (int_parser <* char ',') (int_parser))
      (char ';' *> d.e d)
  in
  let expression d =
    set_parser d
    <|> return_parser d
    <|> if_parser d
    <|> int_token
    <|> for_parser d
  in
  { e = expression }
;;

let e = type_d.e type_d

let parse_exp text =
  let result = Angstrom.parse_string e ~consume:Angstrom.Consume.All text in
  result
;;


let line_parser = 
  lift2 
    (fun num e -> (num, e))
    (number_parser <* space1)
    e
;;

let print_num (a, b) =
  printf "Num: (%i.%i)" a b
;;

let rec print_expr = function
  | Set (id, e) -> 
    Caml.Format.printf "Set: %s ["id;
    print_expr e;
    Caml.Format.print_string "]"
  | If (e, n1, n2, n3) ->
    Caml.Format.print_string "If [";
    print_expr e;
    print_num n1;
    print_num n2;
    print_num n3;
    Caml.Format.print_string "]"
  | Do num ->
    print_string "DO ";
    print_num num
  | Literal (Int x) ->
    Caml.Format.printf "(Int %i) " x
  | For (id, i1, i2, i3, e) ->
    Caml.Format.printf "For %s = (%i, %i, %i) [" id i1 i2 i3;
    print_expr e;
    print_string "]"
  | _ -> () 
;;

let () =
  (*let r = parse_exp "SET x=10" in*)
  (*let r = parse_exp "IF (0) 11.22,11.23,11.24" in*)
  
  let r = parse_exp "FOR x=0,1,10; SET a=1" in
  match r with
  | Result.Ok e -> print_expr e;
  | _ -> Caml.Format.print_string "FAILED";
  ()
;;

(* ----------------------------------------------------------------------- *)
(* ----------------------------------------------------------------------- *)
(* --------------------------------Execution------------------------------ *)
(* ----------------------------------------------------------------------- *)
(* ----------------------------------------------------------------------- *)

(** Lines -- list of commands; cp -- command pointer; ctx -- variables and its values *)

module Data =
  struct
    type t = string
    let compare = compare
end

module CtxData = Map.Make(Data)
let m = CtxData.(empty |> add "a" (Int 10))
let k = m |> CtxData.add "b" (Float 10.0)

let add_to_ctx l new_el = 
  if List.exists (fun )

let exec_op = function 
  | Add, Int v1, Int v2 -> Int (v1 + v2)
  | Sub, Int v1, Int v2 -> Int (v1 - v2)
  | Mul, Int v1, Int v2 -> Int (v1 * v2)
  | Div, Int v1, Int v2 -> Int (v1 / v2)
  | Add, Float v1, Float v2 -> Float (v1 +. v2)
  | Sub, Float v1, Float v2 -> Float (v1 -. v2)
  | Mul, Float v1, Float v2 -> Float (v1 *. v2)
  | Div, Float v1, Float v2 -> Float (v1 /. v2)
  | _ -> failwith "Incorrect operation usage"  
;;

let rec exec_arythm ctx op = 
  let open Base in
  match op with
  | Ident id -> List.find_exn ctx ~f:(fun (i, v) -> equal_string i id) |> snd
  | Literal v -> v
  | Arythm (op, e1, e2) ->
    let v1 = exec_arythm ctx e1 in
    let v2 = exec_arythm ctx e2 in
    exec_op (op, v1, v2)
  | _ -> failwith "Incorrect binary operation syntax"
;;

let rec evaluator lines cp ctx prev =  
  let open Base in
  let current_cmd = List.nth_exn lines cp in

  let find_cmd_index (n1, n2) = 
    List.findi_exn ~f:(fun _ ((n11, n22), _) -> n11 = n1 && n22 = n2) lines
    |> fst 
  in 
  match current_cmd with
  | (_, Set (id, Literal l)) -> 
    evaluator lines (cp + 1) (CtxData.add id l ctx) prev;
  | (_, Do num) ->
    let next = find_cmd_index num in
    let new_ctx = evaluator lines next ctx cp in
    evaluator lines (cp + 1) new_ctx prev
  | (_, Return) -> ctx
  | (_, Goto num) -> 
    let next = find_cmd_index num in
    evaluator lines next ctx prev
  | (_, For (id, i1, i2, i3, Do num)) ->
    let rec exec_for current step endv =
      0
    in

    () 
  | (_, If (e, n1, n2, n3)) ->
    let v = exec_arythm ctx e in
    let next_cmd = match v with
      | Int i when i < 0 -> find_cmd_index n1
      | Int i when i = 0 -> find_cmd_index n2
      | Int i when i > 0 -> find_cmd_index n3
      | _ -> failwith "Incorrect IF command syntax" 
    in
    evaluator lines next_cmd ctx prev
  | _ -> []
 
;;  

let run_program text =
  let lines = Str.split (Str.regexp "[\n]+") text in
  let parsed_lines = List.map (fun x -> 0) lines in
  0
;;
