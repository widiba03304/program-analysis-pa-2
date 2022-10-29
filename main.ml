(*
 * SNU 4541.664A Program Analysis
 * Main program driver
 *)

open Program

let src = ref ""
let opt_pp = ref false

let main () = 
  Arg.parse
    [ ("-pp", Arg.Unit (fun _ -> opt_pp := true), "print pgm") ]
    (fun x -> src := x)
    ("Usage : " ^ (Filename.basename Sys.argv.(0)) ^ " [-option] [filename] ");
  let lexbuf = Lexing.from_channel (if !src = "" then stdin else open_in !src) in
  let pgm = (Parser.program Lexer.start lexbuf) in
  if !opt_pp then (
	  let _ =print_endline "=== Printing Input Program ===" in
	  pp pgm
  )
  else 
    let result = Space.spaceAnalyzer pgm in
    print_endline (Space.result_to_str result)
  
let _ = main ()
