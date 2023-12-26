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
  | For of ident * expr * expr * expr * expr
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
  >>= fun op ->
  let res = match op with
    | "+" -> Add
    | "-" -> Sub
    | "*" -> Mul
    | _ -> Div
  in
  return res
;;

let ident_parser = new_ident >>= fun x -> return @@ Ident x

type dispatch = { e : dispatch -> expr t }

let type_d =  
  let set_parser d =
    fix 
    @@ fun _ ->  
    lift2 
      (fun i e -> Set (i, e))
      (token "SET" *> space1 *> new_ident <* token "=")
      (d.e d)
    <?> "set"
  in
  let return_parser _ =
    space *> token "RETURN"
    >>= fun _ -> return Return
  in
  let do_parser _ =
    token "DO" *> number_parser >>= fun num -> return @@ Do num
  in
  let goto_parser _ =
    token "GOTO" *> number_parser >>= fun num -> return @@ Goto num
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
        (fun a b c -> a, b, c) (d.e d <* char ',') (d.e d <* char ',') (d.e d))
      (char ';' *> d.e d) <?> "For"
  in
  let binop_parser d =
    fix
    @@ fun _ ->
    let par1 = choice [ char '('; char '['; char '<' ] in
    let par2 = choice [ char ')'; char ']'; char '>' ] in
    let c = choice [ char '(' *> d.e d <* char ')'; ident_parser; int_token ] in
    lift3
      (fun e1 op e2 -> Arythm (op, e1, e2))
      (space *> c <* space)
      token_binop_parse
      (space *> c <* space)
    <?> "Binop"
  in
  let expression d =
    set_parser d
    <|> do_parser d
    <|> goto_parser d
    <|> binop_parser d
    <|> ident_parser
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
    e <* space
;;

let print_num (a, b) =
  printf "Num: (%i.%i)" a b
;;

let rec print_expr = function
  | Set (id, e) -> 
    Caml.Format.printf "Set: %s ["id;
    print_expr e;
    Caml.Format.print_string "]"
  | Return ->
    printf "Return"
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
  | Goto num ->
    print_string "GOTO ";
    print_num num
  | Ident id -> printf "Ident: %s; " id;
  | Literal (Int x) ->
    Caml.Format.printf "(Int %i) " x
  | Arythm (op, e1, e2) ->
    printf "Binop [";
    print_expr e1;
    print_expr e2;
    printf "]"
  | For (id, i1, i2, i3, e) ->
    Caml.Format.printf "For %s: [" id;
    print_expr i1;
    print_expr i2;
    print_expr i3;
    print_expr e;
    print_string "]"
  | _ -> () 
;;


let p text =
  let result = Angstrom.parse_string line_parser ~consume:Angstrom.Consume.All text in
  match result with
  | Result.Ok r -> 
    print_expr (snd r);
    printf "\n";
    r
  | _ -> failwith "failed to parse line"
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
  | Ident id -> CtxData.find id ctx (* List.find_exn ctx ~f:(fun (i, v) -> equal_string i id) |> snd *)
  | Literal v -> v
  | Arythm (op, e1, e2) ->
    let v1 = exec_arythm ctx e1 in
    let v2 = exec_arythm ctx e2 in
    exec_op (op, v1, v2)
  | _ -> failwith "Incorrect binary operation syntax"
;;

let print_value = function
  | Int i -> print_int i
  | Float f -> print_float f
  | String s -> print_string s
;;

let print_vars data =
  CtxData.iter (fun id v -> printf "Var: %s Val: " id; print_value v; printf "\n") data
;;

let rec evaluator lines cp ctx =  
  let open Base in
  let current_cmd = List.nth_exn lines cp in

  let find_cmd_index (n1, n2) = 
    List.findi_exn ~f:(fun _ ((n11, n22), _) -> n11 = n1 && n22 = n2) lines
    |> fst 
  in
  printf "DEBUG: ";
  print_expr (snd current_cmd);
  printf "\n";
  print_vars ctx;
    
  match current_cmd with
  | (_, Set (id, e)) ->
    let v = exec_arythm ctx e in
    evaluator lines (cp + 1) (CtxData.add id v ctx);
  | (_, Do num) ->
    let next = find_cmd_index num in
    let new_ctx = evaluator lines next ctx in
    evaluator lines (cp + 1) new_ctx
  | (_, Return) -> ctx
  | (_, Goto num) -> 
    let next = find_cmd_index num in
    evaluator lines next ctx
  | (_, For (id, i1, i2, i3, Do num)) ->
    let next_cmd = find_cmd_index num in
    let rec exec_for current step endv current_ctx =
      if current < endv then
        let new_ctx = evaluator lines next_cmd (CtxData.add id (Int current) current_ctx) in
        exec_for (current + step) step endv new_ctx
      else
        current_ctx
    in
    let v1 = exec_arythm ctx i1 in
    let v2 = exec_arythm ctx i2 in
    let v3 = exec_arythm ctx i3 in
    (match v1, v2, v3 with
    | Int a, Int b, Int c ->
      let new_ctx = exec_for a b c ctx in
      evaluator lines (cp + 1) new_ctx
    | _ -> failwith "Failed to exec FOR") 
  | (_, If (e, n1, n2, n3)) ->
    let v = exec_arythm ctx e in
    let next_cmd = match v with
      | Int i when i < 0 -> find_cmd_index n1
      | Int i when i = 0 -> find_cmd_index n2
      | Int i when i > 0 -> find_cmd_index n3
      | _ -> failwith "Incorrect IF command syntax" 
    in
    evaluator lines next_cmd ctx
  | _ -> CtxData.empty
;;  

let run_program text =
  let lines = 
    Str.split (Str.regexp "[\n]+") text
    (*|> List.filter (fun x -> not @@ is_whitespace x)*)
  in
  List.iter (fun x -> printf "Line: "; print_string x; printf "\n") lines;
  let parsed_lines = List.map p lines in
  let result = evaluator parsed_lines 0 CtxData.empty in
  printf "======================================\nRESULT: \n";
  print_vars result
;;

let text0 =
  {|10.00 SET fact=10
    10.05 SET result=1
    10.07 FOR i=1,1,fact; DO 11.01
    10.09 RETURN
    11.01 SET result=i*result
    11.02 RETURN|}

let text1 =
  {|10.00 IF (1) 11.00,12.00,13.00
    11.00 SET x=10
    11.01 GOTO 14.00
    12.00 SET x=20
    12.01 GOTO 14.00
    13.00 SET x=30
    13.01 GOTO 14.00
    14.00 RETURN|}

    
let get_file filename =
  let current_channel = open_in filename in
  let data = really_input_string current_channel (in_channel_length current_channel) in
  close_in current_channel;
  data
;;

let () =
  let file = Sys.argv.(1) in
  let text = get_file file in
  run_program text
;;