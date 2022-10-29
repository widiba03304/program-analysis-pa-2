(*
   SNU 4541.664A Program Analysis 
   K-- Lexer
*)

{
 open Parser
 exception Eof
 exception LexicalError
 let verbose1 s =  (* (print_string s; print_newline(); s) *) s
 let verbose2 s =  (* (print_string s; print_newline()) *) ()
 let comment_depth = ref 0
 let keyword_tbl = Hashtbl.create 31
 let _ = List.iter (fun (keyword, tok) -> Hashtbl.add keyword_tbl keyword tok)
                   [
                    ("if", IF);
                    ("then", THEN);
                    ("else", ELSE);
					("repeat", REPEAT);
					("until", UNTIL);
					("readInt", READINT)
                  ] 
} 

let blank = [' ' '\n' '\t' '\r']+
let id = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '\'' '0'-'9' '_']*
let number = ['0'-'9']+

rule start =
 parse blank { start lexbuf }
     | "(*" { comment_depth :=1;
              comment lexbuf;
              start lexbuf }
     | number { NUM (int_of_string (verbose1 (Lexing.lexeme lexbuf))) }
     | id { let id = verbose1 (Lexing.lexeme lexbuf)
            in try Hashtbl.find keyword_tbl id
               with _ -> ID id
            }
	 | "+" {verbose2 "+"; PLUS}
     | "-" {verbose2 "-"; MINUS}
     | ":=" {verbose2 ":="; COLONEQ}
     | ";" { verbose2 ";"; SEMICOLON}
     | "(" { verbose2 "("; LP}
     | ")" { verbose2 ")"; RP}
     | "," { verbose2 ","; COMMA}
     | eof { verbose2 "eof"; EOF}
     | _ {raise LexicalError}

and comment = parse
     "(*" {comment_depth := !comment_depth+1; comment lexbuf}
   | "*)" {comment_depth := !comment_depth-1;
           if !comment_depth > 0 then comment lexbuf }
   | eof {raise Eof}
   | _   {comment lexbuf}
