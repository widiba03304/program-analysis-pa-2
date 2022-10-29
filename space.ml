(*
 * SNU 4541.664A Program Analysis 2022 Fall
 * DongKwon Lee (dklee@ropas.snu.ac.kr)
 *)


(*
 * space analyzer
 *)
 
open Program 

exception Error of string
 
type result =
  | YES         (* all var has value 0~1 mod 8 *)
  | NO          (* somve var doesn't has value 0~1 mod 8 *)
  | DONTKNOW    (* can't determine *)

let result_to_str : result -> string = fun a -> match a with
  | YES -> "Yes"
  | NO -> "No"
  | DONTKNOW -> "I don't know"
  
(* space analysis *)
let rec spaceAnalyzer : program -> result = fun pgm ->
  raise (Error ("not impled"))
