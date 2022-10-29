(*
 * SNU 4541.664A Program Analysis 2022 FALL
 * DongKwon Lee (dklee@ropas.snu.ac.kr)
 *)


(*
 * program
 *)
  
type id = string
type exp =
  | NUM of int
  | VAR of id
  | ADD of exp * exp
  | MINUS of exp
  | READINT of (int * int)
  
type cmd =
  | SEQ of cmd * cmd        (* sequence *)
  | IF of exp * cmd * cmd   (* if-then-else *)
  | REPEAT of cmd * exp     (* repeat *)
  | ASSIGN of id * exp      (* assgin to variable *)

type program = cmd

let rec exp_to_str = function
  | NUM i -> string_of_int i
  | VAR x -> x
  | ADD (e1, e2) -> "(" ^ exp_to_str e1 ^ " + " ^ (exp_to_str e2) ^ ")"
  | MINUS e -> "-(" ^ exp_to_str e ^ ")"
  | READINT (n1, n2) -> "READINT(" ^ string_of_int n1 ^ ", " ^ string_of_int n2 ^ ")"

let rec cmd_to_str = function
  | ASSIGN (x, e) -> x ^ " := " ^ exp_to_str e
  | SEQ (c1, c2) -> cmd_to_str c1 ^ ";\n" ^ cmd_to_str c2
  | IF (e, c1, c2) -> 
    "if (" ^ exp_to_str e ^ ")" ^ cmd_to_str c1 ^ "\n" ^ cmd_to_str c2
  | REPEAT (c, e) -> 
    "repeat (" ^ cmd_to_str c ^ ") until (" ^ exp_to_str e ^ ")"



let q x = ["\"" ^ x ^ "\""]
let pfx = "  "
let indent l = List.map (fun s -> pfx ^ s) l
let rec comma = function [] -> []
  | [h] -> [h ^ ","]
  | (h :: t) -> h :: (comma t)
let rec qs xs = match xs with
	[] -> []
	| [hd] -> (q hd)
	| hd::tl -> (comma (q hd))@(qs tl)
let ps s l = 
	match l with 
	  [] -> [s]
	| (h :: t) -> 
		(s ^ "(")
      		:: (List.fold_left (fun l x -> (comma l) @ (indent x)) (indent h) t)
      		@ [(")")]

let rec id_e (id,e) = (q id)@(pe e)
and pe e =
    match e with
      NUM i -> ps "NUM" [[string_of_int i]]
    | VAR x -> ps "VAR" [q x]
	| ADD (e1, e2) -> ps "ADD" [pe e1; pe e2]
    | MINUS e1 -> ps "MINUS" [pe e1]
	| READINT (n1, n2) -> ps "READINT" [[string_of_int n1; string_of_int n2]]
and pc c =
	match c with
	| SEQ (c1, c2) -> ps "SEQ" [pc c1; pc c2]
    | IF (e, c1, c2) -> ps "IF" [pe e; pc c1; pc c2]
    | ASSIGN (i, e) -> ps "ASSIGN" [q i; pe e]
	| REPEAT (c1, e) -> ps "REPEAT" [pc c1; pe e]

let pp c =  List.iter print_endline (pc c)


