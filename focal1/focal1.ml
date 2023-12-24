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
  | Ident of ident
  | Return
  | Type of expr
  | If of expr * number * number * number
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
  let expression d =
    set_parser d
    <|> return_parser d
    <|> if_parser d
    <|> int_token
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
  | Literal (Int x) ->
    Caml.Format.printf "(Int %i) " x
  | _ -> () 
;;

let () =
  (*let r = parse_exp "SET x=10" in*)
  let r = parse_exp "IF (0) 11.22,11.23,11.24" in
  match r with
  | Result.Ok e -> print_expr e;
  | _ -> Caml.Format.print_string "FAILED";
  ()
;;


(* ------------------------------- *)
(* -------------Exec-------------- *)
(* ------------------------------- *)
let rec evaluator lines =  
  0
;;  

let run_program text =
  let lines = Str.split (Str.regexp "[\n]+") text in
  let parsed_lines = List.map (fun x -> 0) lines in
  0
;;
